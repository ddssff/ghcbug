{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}

module SiteMap
    ( WhichList(..)
    , ElemID(..)
    , ListOp(ListOpReorder)
    , Path_ReportMap(..)
    , Path_Report(..)
    , Path_ReportView(..)
    ) where

import Data.Data ( Data )
import Data.Typeable ( Typeable )
import Network.URI ( URI(uriPath), nullURI )
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint) )
import Control.Lens
    ( iso, _Just, _1, _2, _Left, _Right, Lens', Traversal' )
import Data.Int ( Int64 )
import Data.Map ( Map )
import Data.Text ( Text )
import Data.UserId ( UserId )
import Data.UUID ( UUID )
import Data.UUID.Orphans ()

import Appraisal.CIString ( CIString(..) )
import Appraisal.Image ( ImageCrop, Units, ImageSize(ImageSize), Dimension )
import Appraisal.ImageFile ( ImageFile )
import Appraisal.IntJS ( JSONText, ToIntJS(intJS) )
import Appraisal.Markup ( Markup(..) )
import Appraisal.Permissions ( UserIds, Permissions(Permissions) )
import Appraisal.Report as Report
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
      Report(Report),
      ReportFlags(ReportFlags),
      ReportElem(ReportItem, ReportParagraph, ReportUndecided),
      Author(Author) )
import Appraisal.ReportImage ( ReportImageID, ReportImage(Pic), MaybeImageFile, ReportImages )
import Appraisal.ReportInstances
    ( SaneSize,
      ReadOnlyFilePath,
      ReportView(ReportView),
      SaneSizeImageSize,
      ReportImageView(ReportImageView) )
import Appraisal.ReportItem ( Item(Item), ItemFieldName )
import Appraisal.ReportMap (ReportID(..), ReportMap(..))

import Language.Haskell.TH.Path.Core
    ( Path_Maybe(Path_Just),
      Path_OMap(Path_At),
      Path_Map(Path_Look),
      Path_Either(Path_Left, Path_Right),
      Path_Pair(Path_First, Path_Second),
      Path(..),
      IdPath(..),
      mat )
import Language.Haskell.TH.Path.Order ( lens_omat )
import Language.Haskell.TH.Path.View ( View(viewLens) )

data ReportLens = ReportLens deriving (Eq, Ord, Read, Show, Typeable, Data)


data ReportAction = View | ProfileLens ProfileDataMap | ItemLens ItemLensURL
                    deriving (Eq, Ord, Read, Show, Typeable, Data)

data ProfileDataMap = Username
                    | Email
                    | OptOut
                    | Roles
                      deriving (Read, Show, Eq, Ord, Typeable, Data)

data ItemLensURL =
          ItemList
        | ItemShort Int
        | ItemLong Int
          deriving (Read, Show, Eq, Ord, Typeable, Data)

data SiteMap = SiteMap_PlaceHolder
               deriving (Eq, Ord, Read, Show, Typeable, Data)

data AppraisalURL = A_Html AppraisalPage
                  | A_Partial AppraisalPage
                  | A_Lens (Path_ReportMap JSONText)
                  | A_ImageUpload (Path_ReportMap JSONText)
                  | A_ImageSideload (Path_ReportMap JSONText)
                  | A_List ListOp
                  | A_PlaceHolder String
                  | A_File FilePath
                  | A_File2 FilePath FilePath
                  | ImageDir String
                  | JavaScript FilePath
                  | ProfileData ProfileDataMap
                  | TODO String
                  | A_NullURL
                  deriving (Eq, Ord, Read, Show, Typeable, Data)

data AppraisalPage = AP_Home
                   | AP_MainTabSet
                   | AP_ReportList
                   | AP_ReportCreate
                   | AP_ReportCopy ReportID
                   | AP_ReportDelete ReportID
                   | AP_Report ReportID
                   | AP_ReportTab ReportID ReportTab
                   | AP_ItemList ReportID
                   | AP_ItemPage ReportID ItemPage
                   | AP_ReportLatex ReportID
                     deriving (Eq, Ord, Read, Show, Typeable, Data)

data ItemPage = ItemThumbnail Report.ReportElemID
              | ItemEditor Report.ReportElemID
              | ItemWhichList WhichList Report.ReportElemID
              deriving (Eq, Ord, Read, Show, Typeable, Data)

newtype WhichConstructor = WhichConstructor Int
                         deriving (Eq, Ord, Read, Show, Typeable, Data)

data ListOp
    = ListOpNew ReportID WhichList WhichConstructor
    | ListOpDel ReportID WhichList ElemID
    | ListOpReorder ReportID WhichList
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

data ElemID
    = ReportImageID {unReportImageID :: ReportImageID}
    | AuthorID {unAuthorID :: Report.AuthorID}
    | MarkupID {unMarkupID :: Report.MarkupID}
    | MarkupPairID {unMarkupPairID :: Report.MarkupPairID}
    | AbbrevPairID {unAbbrevPairID :: Report.AbbrevPairID}
    | ElemID {unElemID :: Report.ReportElemID}
    deriving (Eq, Ord, Read, Show, Typeable, Data)

instance Pretty ElemID where
    pPrint (SiteMap.ReportImageID x) = pPrint x
    pPrint (SiteMap.AuthorID x) = pPrint x
    pPrint (SiteMap.MarkupID x) = pPrint x
    pPrint (SiteMap.MarkupPairID x) = pPrint x
    pPrint (SiteMap.AbbrevPairID x) = pPrint x
    pPrint (SiteMap.ElemID x) = pPrint x

instance ToIntJS ElemID where
    intJS x@(SiteMap.ReportImageID {}) = intJS . SiteMap.unReportImageID $ x
    intJS x@(SiteMap.AuthorID {}) = intJS . SiteMap.unAuthorID $ x
    intJS x@(SiteMap.MarkupID {}) = intJS . SiteMap.unMarkupID $ x
    intJS x@(SiteMap.MarkupPairID {}) = intJS . SiteMap.unMarkupPairID $ x
    intJS x@(SiteMap.AbbrevPairID {}) = intJS . SiteMap.unAbbrevPairID $ x
    intJS x@(SiteMap.ElemID {}) = intJS . SiteMap.unElemID $ x

data ReportTab = RT_TitlePage
               | RT_AppraiserTab
               | RT_ClientInformationTab
               | RT_AppraisalTab
               | RT_LetterOfTransmittalTab
               | RT_ScopeOfWorkTab
               | RT_PermissionsTab
               | RT_LimitingConditionsTab
               | RT_PrivacyPolicyTab
               | RT_AuthorsCredentialsTab
               | RT_CertificationTab
               | RT_GlossaryTab
               | RT_AbbrevsTab
               | RT_SourcesTab
               | RT_ItemsTab
               | RT_ValueTab
               | RT_ItemEditor Report.ReportElemID
               deriving (Eq, Ord, Read, Show, Typeable, Data)

data TestURL = TestHome
             | TestCropper
             | TestDynamicLoad
             | TestMultiText
             | TestTimeStamp
             | TestIFrame Int
             | TestGenericTabs
             | TestToggle
             | TestStoplight
             | TestListUI
             | TestListUI2
             | TestBigList2
             | TestListContent Int
             deriving (Eq, Ord, Read, Show, Typeable, Data)

class ToURL a where
    type URLT a
    nullURL :: a
    errorURL :: a -> URLT a

instance ToURL AppraisalURL where
  type URLT AppraisalURL = URI
  nullURL = A_NullURL
  errorURL _ = globalErrorURI

globalErrorURI :: URI
globalErrorURI = nullURI { uriPath = "/badurl" }

data Path_Author a
    = Path_Author_authorName (Path_Markup a)
    | Path_Author_authorCredentials (Path_Markup a)
    | Path_Author
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Bool a
    = Path_Bool_View (Path_String a) | Path_Bool
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Branding a
    = Path_Branding_View (Path_Text a) | Path_Branding
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_CIString a
    = Path_CIString_View (Path_Text a) | Path_CIString
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Dimension a
    = Path_Dimension_View (Path_JSONText a) | Path_Dimension
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Double a
    = Path_Double_View (Path_String a) | Path_Double
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ImageCrop a
    = Path_ImageCrop
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ImageFile a
    = Path_ImageFile
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ImageSize a
    = Path_ImageSize_dim (Path_Dimension a)
    | Path_ImageSize_size (Path_Double a)
    | Path_ImageSize_units (Path_Units a)
    | Path_ImageSize
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Int64 a
    = Path_Int64
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Integer a
    = Path_Integer
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Item a
    = Path_Item_itemName (Path_Text a)
    | Path_Item_fields (Path_Map ItemFieldName (Path_Markup a))
    | Path_Item_images (Path_ReportImages a)
    | Path_Item
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_JSONText a
    = Path_JSONText
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Markup a
    = Path_Markup_markdownText (Path_Text a)
    | Path_Markup_htmlText (Path_Text a)
    | Path_Markup
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_MaybeImageFile a
    = Path_MaybeImageFile_View (Path_String a) | Path_MaybeImageFile
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_MaybeReportIntendedUse a
    = Path_MaybeReportIntendedUse_View (Path_String a)
    | Path_MaybeReportIntendedUse
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Permissions a
    = Path_Permissions_owner (Path_UserId a)
    | Path_Permissions_writers (Path_UserIds a)
    | Path_Permissions_readers (Path_UserIds a)
    | Path_Permissions
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ReadOnlyFilePath a
    = Path_ReadOnlyFilePath_View a | Path_ReadOnlyFilePath
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Report a
    = Path_Report_View (Path_ReportView a) | Path_Report
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ReportElem a
    = Path_ReportElem_elemItem (Path_Item a)
    | Path_ReportElem_elemText (Path_Markup a)
    | Path_ReportElem
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ReportFlags a
    = Path_ReportFlags_hideEmptyItemFields (Path_Bool a)
    | Path_ReportFlags
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ReportImage a
    = Path_ReportImage_View (Path_ReportImageView a) | Path_ReportImage
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ReportImageView a
    = Path_ReportImageView__picSize (Path_SaneSizeImageSize a)
    | Path_ReportImageView__picCrop (Path_ImageCrop a)
    | Path_ReportImageView__picCaption (Path_Markup a)
    | Path_ReportImageView__picOriginal (Path_Maybe (Path_Either (Path_URI a)
                                                                 (Path_ImageFile a)))
    | Path_ReportImageView__picEditedDeprecated (Path_MaybeImageFile a)
    | Path_ReportImageView__picThumbDeprecated (Path_MaybeImageFile a)
    | Path_ReportImageView__picPrinterDeprecated (Path_MaybeImageFile a)
    | Path_ReportImageView__picMustEnlarge (Path_Bool a)
    | Path_ReportImageView__picEnlargedDeprecated (Path_MaybeImageFile a)
    | Path_ReportImageView
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ReportIntendedUse a
    = Path_ReportIntendedUse_View (Path_String a)
    | Path_ReportIntendedUse
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ReportMap a
    = Path_ReportMap_unReportMap (Path_Map ReportID (Path_Report a))
    | Path_ReportMap
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ReportStatus a
    = Path_ReportStatus_View (Path_String a) | Path_ReportStatus
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ReportValueApproachInfo a
    = Path_ReportValueApproachInfo_reportValueApproachName (Path_Markup a)
    | Path_ReportValueApproachInfo_reportValueApproachDescription (Path_Markup a)
    | Path_ReportValueApproachInfo
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ReportValueTypeInfo a
    = Path_ReportValueTypeInfo_reportValueTypeName (Path_Markup a)
    | Path_ReportValueTypeInfo_reportValueTypeDescription (Path_Markup a)
    | Path_ReportValueTypeInfo_reportValueTypeDefinition (Path_Markup a)
    | Path_ReportValueTypeInfo
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ReportView a
    = Path_ReportView__reportFolder (Path_ReadOnlyFilePath a)
    | Path_ReportView__reportName (Path_Markup a)
    | Path_ReportView__reportDate (Path_Markup a)
    | Path_ReportView__reportContractDate (Path_Markup a)
    | Path_ReportView__reportInspectionDate (Path_Markup a)
    | Path_ReportView__reportEffectiveDate (Path_Markup a)
    | Path_ReportView__reportAuthors (Path_Authors a)
    | Path_ReportView__reportPreparer (Path_Markup a)
    | Path_ReportView__reportPreparerEIN (Path_Markup a)
    | Path_ReportView__reportPreparerAddress (Path_Markup a)
    | Path_ReportView__reportPreparerEMail (Path_Markup a)
    | Path_ReportView__reportPreparerWebsite (Path_Markup a)
    | Path_ReportView__reportAbbrevs (Path_AbbrevPairs a)
    | Path_ReportView__reportTitle (Path_Markup a)
    | Path_ReportView__reportHeader (Path_Markup a)
    | Path_ReportView__reportFooter (Path_Markup a)
    | Path_ReportView__reportIntendedUse (Path_MaybeReportIntendedUse a)
    | Path_ReportView__reportValueTypeInfo (Path_ReportValueTypeInfo a)
    | Path_ReportView__reportValueApproachInfo (Path_ReportValueApproachInfo a)
    | Path_ReportView__reportClientName (Path_Markup a)
    | Path_ReportView__reportClientAddress (Path_Markup a)
    | Path_ReportView__reportClientGreeting (Path_Markup a)
    | Path_ReportView__reportItemsOwnerFull (Path_Markup a)
    | Path_ReportView__reportItemsOwner (Path_Markup a)
    | Path_ReportView__reportBriefItems (Path_Markup a)
    | Path_ReportView__reportInspectionLocation (Path_Markup a)
    | Path_ReportView__reportBody (Path_ReportElems a)
    | Path_ReportView__reportGlossary (Path_MarkupPairs a)
    | Path_ReportView__reportSources (Path_MarkupPairs a)
    | Path_ReportView__reportLetterOfTransmittal (Path_Markup a)
    | Path_ReportView__reportScopeOfWork (Path_Markup a)
    | Path_ReportView__reportCertification (Path_Markups a)
    | Path_ReportView__reportLimitingConditions (Path_Markups a)
    | Path_ReportView__reportPrivacyPolicy (Path_Markup a)
    | Path_ReportView__reportPerms (Path_Permissions a)
    | Path_ReportView__reportRevision (Path_Integer a)
    | Path_ReportView__reportCreated (Path_EpochMilli a)
    | Path_ReportView__reportBranding (Path_Branding a)
    | Path_ReportView__reportStatus (Path_ReportStatus a)
    | Path_ReportView__reportRedacted (Path_Bool a)
    | Path_ReportView__reportFlags (Path_ReportFlags a)
    | Path_ReportView__reportUUID (Path_UUID a)
    | Path_ReportView__reportOrderByItemName (Path_Bool a)
    | Path_ReportView__reportDisplayItemName (Path_Bool a)
    | Path_ReportView
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_SaneSizeImageSize a
    = Path_SaneSizeImageSize_View (Path_ImageSize a)
    | Path_SaneSizeImageSize
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_String a
    = Path_String_View (Path_JSONText a) | Path_String
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Text a
    = Path_Text_View (Path_JSONText a) | Path_Text
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_URI a
    = Path_URI
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_UUID a
    = Path_UUID
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Units a
    = Path_Units_View (Path_JSONText a) | Path_Units
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_UserId a
    = Path_UserId
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_UserIds a
    = Path_UserIds_View (Path_Text a) | Path_UserIds
    deriving (Eq, Ord, Read, Show, Typeable, Data)
instance IdPath (Path_Author a)
    where idPath = Path_Author
instance IdPath (Path_Bool a)
    where idPath = Path_Bool
instance IdPath (Path_Branding a)
    where idPath = Path_Branding
instance IdPath (Path_CIString a)
    where idPath = Path_CIString
instance IdPath (Path_Dimension a)
    where idPath = Path_Dimension
instance IdPath (Path_Double a)
    where idPath = Path_Double
instance IdPath (Path_ImageCrop a)
    where idPath = Path_ImageCrop
instance IdPath (Path_ImageFile a)
    where idPath = Path_ImageFile
instance IdPath (Path_ImageSize a)
    where idPath = Path_ImageSize
instance IdPath (Path_Int64 a)
    where idPath = Path_Int64
instance IdPath (Path_Integer a)
    where idPath = Path_Integer
instance IdPath (Path_Item a)
    where idPath = Path_Item
instance IdPath (Path_JSONText a)
    where idPath = Path_JSONText
instance IdPath (Path_Markup a)
    where idPath = Path_Markup
instance IdPath (Path_MaybeImageFile a)
    where idPath = Path_MaybeImageFile
instance IdPath (Path_MaybeReportIntendedUse a)
    where idPath = Path_MaybeReportIntendedUse
instance IdPath (Path_Permissions a)
    where idPath = Path_Permissions
instance IdPath (Path_ReadOnlyFilePath a)
    where idPath = Path_ReadOnlyFilePath
instance IdPath (Path_Report a)
    where idPath = Path_Report
instance IdPath (Path_ReportElem a)
    where idPath = Path_ReportElem
instance IdPath (Path_ReportFlags a)
    where idPath = Path_ReportFlags
instance IdPath (Path_ReportImage a)
    where idPath = Path_ReportImage
instance IdPath (Path_ReportImageView a)
    where idPath = Path_ReportImageView
instance IdPath (Path_ReportIntendedUse a)
    where idPath = Path_ReportIntendedUse
instance IdPath (Path_ReportMap a)
    where idPath = Path_ReportMap
instance IdPath (Path_ReportStatus a)
    where idPath = Path_ReportStatus
instance IdPath (Path_ReportValueApproachInfo a)
    where idPath = Path_ReportValueApproachInfo
instance IdPath (Path_ReportValueTypeInfo a)
    where idPath = Path_ReportValueTypeInfo
instance IdPath (Path_ReportView a)
    where idPath = Path_ReportView
instance IdPath (Path_SaneSizeImageSize a)
    where idPath = Path_SaneSizeImageSize
instance IdPath (Path_String a)
    where idPath = Path_String
instance IdPath (Path_Text a)
    where idPath = Path_Text
instance IdPath (Path_URI a)
    where idPath = Path_URI
instance IdPath (Path_UUID a)
    where idPath = Path_UUID
instance IdPath (Path_Units a)
    where idPath = Path_Units
instance IdPath (Path_UserId a)
    where idPath = Path_UserId
instance IdPath (Path_UserIds a)
    where idPath = Path_UserIds
type Path_AbbrevPairs a = Path_OMap AbbrevPairID
                                    (Path_Pair (Path_CIString a) (Path_Markup a))
type Path_Authors a = Path_OMap AuthorID (Path_Author a)
type Path_EpochMilli a = Path_Int64 a
type Path_MarkupPairs a = Path_OMap MarkupPairID
                                    (Path_Pair (Path_Markup a) (Path_Markup a))
type Path_Markups a = Path_OMap MarkupID (Path_Markup a)
type Path_ReportElems a = Path_OMap ReportElemID
                                    (Path_ReportElem a)
type Path_ReportImages a = Path_OMap ReportImageID
                                     (Path_ReportImage a)
class HasAuthor c
    where lens_author :: Lens' c Author
          lens_Author_authorCredentials :: forall . Lens' c Markup
          lens_Author_authorCredentials = (.) lens_author lens_Author_authorCredentials
          {-# INLINE lens_Author_authorCredentials #-}
          lens_Author_authorName :: forall . Lens' c Markup
          lens_Author_authorName = (.) lens_author lens_Author_authorName
          {-# INLINE lens_Author_authorName #-}
class HasBool c
    where lens_bool :: Lens' c Bool
class HasBranding c
    where lens_branding :: Lens' c Branding
class HasCIString c
    where cIString :: Lens' c CIString
          lens_CIString_unCIString :: forall . Lens' c String
          lens_CIString_unCIString = (.) cIString lens_CIString_unCIString
          {-# INLINE lens_CIString_unCIString #-}
class HasDimension c
    where lens_dimension :: Lens' c Dimension
class HasDouble c
    where lens_double :: Lens' c Double
class HasImageSize c
    where lens_imageSize :: Lens' c ImageSize
          lens_ImageSize_dim :: forall . Lens' c Dimension
          lens_ImageSize_dim = (.) lens_imageSize lens_ImageSize_dim
          {-# INLINE lens_ImageSize_dim #-}
          lens_ImageSize_size :: forall . Lens' c Double
          lens_ImageSize_size = (.) lens_imageSize lens_ImageSize_size
          {-# INLINE lens_ImageSize_size #-}
          lens_ImageSize_units :: forall . Lens' c Units
          lens_ImageSize_units = (.) lens_imageSize lens_ImageSize_units
          {-# INLINE lens_ImageSize_units #-}
class HasItem c
    where lens_item :: Lens' c Item
          lens_Item_fields :: forall . Lens' c (Map ItemFieldName Markup)
          lens_Item_fields = (.) lens_item lens_Item_fields
          {-# INLINE lens_Item_fields #-}
          lens_Item_images :: forall . Lens' c ReportImages
          lens_Item_images = (.) lens_item lens_Item_images
          {-# INLINE lens_Item_images #-}
          lens_Item_itemName :: forall . Lens' c Text
          lens_Item_itemName = (.) lens_item lens_Item_itemName
          {-# INLINE lens_Item_itemName #-}
class HasMarkup c
    where lens_markup :: Lens' c Markup
          lens_Markup_htmlText :: forall . Traversal' c Text
          lens_Markup_htmlText = (.) lens_markup lens_Markup_htmlText
          {-# INLINE lens_Markup_htmlText #-}
          lens_Markup_markdownText :: forall . Traversal' c Text
          lens_Markup_markdownText = (.) lens_markup lens_Markup_markdownText
          {-# INLINE lens_Markup_markdownText #-}
class HasPermissions c
    where lens_permissions :: Lens' c Permissions
          lens_Permissions_owner :: forall . Lens' c UserId
          lens_Permissions_owner = (.) lens_permissions lens_Permissions_owner
          {-# INLINE lens_Permissions_owner #-}
          lens_Permissions_readers :: forall . Lens' c UserIds
          lens_Permissions_readers = (.) lens_permissions lens_Permissions_readers
          {-# INLINE lens_Permissions_readers #-}
          lens_Permissions_writers :: forall . Lens' c UserIds
          lens_Permissions_writers = (.) lens_permissions lens_Permissions_writers
          {-# INLINE lens_Permissions_writers #-}
class HasReport c
    where lens_report :: Lens' c Report
          lens_Report_reportAbbrevs :: forall . Lens' c AbbrevPairs
          lens_Report_reportAbbrevs = (.) lens_report lens_Report_reportAbbrevs
          {-# INLINE lens_Report_reportAbbrevs #-}
          lens_Report_reportAuthors :: forall . Lens' c Authors
          lens_Report_reportAuthors = (.) lens_report lens_Report_reportAuthors
          {-# INLINE lens_Report_reportAuthors #-}
          lens_Report_reportBody :: forall . Lens' c ReportElems
          lens_Report_reportBody = (.) lens_report lens_Report_reportBody
          {-# INLINE lens_Report_reportBody #-}
          lens_Report_reportBranding :: forall . Lens' c Branding
          lens_Report_reportBranding = (.) lens_report lens_Report_reportBranding
          {-# INLINE lens_Report_reportBranding #-}
          lens_Report_reportBriefItems :: forall . Lens' c Markup
          lens_Report_reportBriefItems = (.) lens_report lens_Report_reportBriefItems
          {-# INLINE lens_Report_reportBriefItems #-}
          lens_Report_reportCertification :: forall . Lens' c Markups
          lens_Report_reportCertification = (.) lens_report lens_Report_reportCertification
          {-# INLINE lens_Report_reportCertification #-}
          lens_Report_reportClientAddress :: forall . Lens' c Markup
          lens_Report_reportClientAddress = (.) lens_report lens_Report_reportClientAddress
          {-# INLINE lens_Report_reportClientAddress #-}
          lens_Report_reportClientGreeting :: forall . Lens' c Markup
          lens_Report_reportClientGreeting = (.) lens_report lens_Report_reportClientGreeting
          {-# INLINE lens_Report_reportClientGreeting #-}
          lens_Report_reportClientName :: forall . Lens' c Markup
          lens_Report_reportClientName = (.) lens_report lens_Report_reportClientName
          {-# INLINE lens_Report_reportClientName #-}
          lens_Report_reportContractDate :: forall . Lens' c Markup
          lens_Report_reportContractDate = (.) lens_report lens_Report_reportContractDate
          {-# INLINE lens_Report_reportContractDate #-}
          lens_Report_reportCreated :: forall . Lens' c EpochMilli
          lens_Report_reportCreated = (.) lens_report lens_Report_reportCreated
          {-# INLINE lens_Report_reportCreated #-}
          lens_Report_reportDate :: forall . Lens' c Markup
          lens_Report_reportDate = (.) lens_report lens_Report_reportDate
          {-# INLINE lens_Report_reportDate #-}
          lens_Report_reportDisplayItemName :: forall . Lens' c Bool
          lens_Report_reportDisplayItemName = (.) lens_report lens_Report_reportDisplayItemName
          {-# INLINE lens_Report_reportDisplayItemName #-}
          lens_Report_reportEffectiveDate :: forall . Lens' c Markup
          lens_Report_reportEffectiveDate = (.) lens_report lens_Report_reportEffectiveDate
          {-# INLINE lens_Report_reportEffectiveDate #-}
          lens_Report_reportFlags :: forall . Lens' c ReportFlags
          lens_Report_reportFlags = (.) lens_report lens_Report_reportFlags
          {-# INLINE lens_Report_reportFlags #-}
          lens_Report_reportFolder :: forall . Lens' c FilePath
          lens_Report_reportFolder = (.) lens_report lens_Report_reportFolder
          {-# INLINE lens_Report_reportFolder #-}
          lens_Report_reportFooter :: forall . Lens' c Markup
          lens_Report_reportFooter = (.) lens_report lens_Report_reportFooter
          {-# INLINE lens_Report_reportFooter #-}
          lens_Report_reportGlossary :: forall . Lens' c MarkupPairs
          lens_Report_reportGlossary = (.) lens_report lens_Report_reportGlossary
          {-# INLINE lens_Report_reportGlossary #-}
          lens_Report_reportHeader :: forall . Lens' c Markup
          lens_Report_reportHeader = (.) lens_report lens_Report_reportHeader
          {-# INLINE lens_Report_reportHeader #-}
          lens_Report_reportInspectionDate :: forall . Lens' c Markup
          lens_Report_reportInspectionDate = (.) lens_report lens_Report_reportInspectionDate
          {-# INLINE lens_Report_reportInspectionDate #-}
          lens_Report_reportInspectionLocation :: forall . Lens' c Markup
          lens_Report_reportInspectionLocation = (.) lens_report lens_Report_reportInspectionLocation
          {-# INLINE lens_Report_reportInspectionLocation #-}
          lens_Report_reportIntendedUse :: forall . Lens' c
                                                          MaybeReportIntendedUse
          lens_Report_reportIntendedUse = (.) lens_report lens_Report_reportIntendedUse
          {-# INLINE lens_Report_reportIntendedUse #-}
          lens_Report_reportItemsOwner :: forall . Lens' c Markup
          lens_Report_reportItemsOwner = (.) lens_report lens_Report_reportItemsOwner
          {-# INLINE lens_Report_reportItemsOwner #-}
          lens_Report_reportItemsOwnerFull :: forall . Lens' c Markup
          lens_Report_reportItemsOwnerFull = (.) lens_report lens_Report_reportItemsOwnerFull
          {-# INLINE lens_Report_reportItemsOwnerFull #-}
          lens_Report_reportLetterOfTransmittal :: forall . Lens' c Markup
          lens_Report_reportLetterOfTransmittal = (.) lens_report lens_Report_reportLetterOfTransmittal
          {-# INLINE lens_Report_reportLetterOfTransmittal #-}
          lens_Report_reportLimitingConditions :: forall . Lens' c Markups
          lens_Report_reportLimitingConditions = (.) lens_report lens_Report_reportLimitingConditions
          {-# INLINE lens_Report_reportLimitingConditions #-}
          lens_Report_reportName :: forall . Lens' c Markup
          lens_Report_reportName = (.) lens_report lens_Report_reportName
          {-# INLINE lens_Report_reportName #-}
          lens_Report_reportOrderByItemName :: forall . Lens' c Bool
          lens_Report_reportOrderByItemName = (.) lens_report lens_Report_reportOrderByItemName
          {-# INLINE lens_Report_reportOrderByItemName #-}
          lens_Report_reportPerms :: forall . Lens' c Permissions
          lens_Report_reportPerms = (.) lens_report lens_Report_reportPerms
          {-# INLINE lens_Report_reportPerms #-}
          lens_Report_reportPreparer :: forall . Lens' c Markup
          lens_Report_reportPreparer = (.) lens_report lens_Report_reportPreparer
          {-# INLINE lens_Report_reportPreparer #-}
          lens_Report_reportPreparerAddress :: forall . Lens' c Markup
          lens_Report_reportPreparerAddress = (.) lens_report lens_Report_reportPreparerAddress
          {-# INLINE lens_Report_reportPreparerAddress #-}
          lens_Report_reportPreparerEIN :: forall . Lens' c Markup
          lens_Report_reportPreparerEIN = (.) lens_report lens_Report_reportPreparerEIN
          {-# INLINE lens_Report_reportPreparerEIN #-}
          lens_Report_reportPreparerEMail :: forall . Lens' c Markup
          lens_Report_reportPreparerEMail = (.) lens_report lens_Report_reportPreparerEMail
          {-# INLINE lens_Report_reportPreparerEMail #-}
          lens_Report_reportPreparerWebsite :: forall . Lens' c Markup
          lens_Report_reportPreparerWebsite = (.) lens_report lens_Report_reportPreparerWebsite
          {-# INLINE lens_Report_reportPreparerWebsite #-}
          lens_Report_reportPrivacyPolicy :: forall . Lens' c Markup
          lens_Report_reportPrivacyPolicy = (.) lens_report lens_Report_reportPrivacyPolicy
          {-# INLINE lens_Report_reportPrivacyPolicy #-}
          lens_Report_reportRedacted :: forall . Lens' c Bool
          lens_Report_reportRedacted = (.) lens_report lens_Report_reportRedacted
          {-# INLINE lens_Report_reportRedacted #-}
          lens_Report_reportRevision :: forall . Lens' c Integer
          lens_Report_reportRevision = (.) lens_report lens_Report_reportRevision
          {-# INLINE lens_Report_reportRevision #-}
          lens_Report_reportScopeOfWork :: forall . Lens' c Markup
          lens_Report_reportScopeOfWork = (.) lens_report lens_Report_reportScopeOfWork
          {-# INLINE lens_Report_reportScopeOfWork #-}
          lens_Report_reportSources :: forall . Lens' c MarkupPairs
          lens_Report_reportSources = (.) lens_report lens_Report_reportSources
          {-# INLINE lens_Report_reportSources #-}
          lens_Report_reportStatus :: forall . Lens' c ReportStatus
          lens_Report_reportStatus = (.) lens_report lens_Report_reportStatus
          {-# INLINE lens_Report_reportStatus #-}
          lens_Report_reportTitle :: forall . Lens' c Markup
          lens_Report_reportTitle = (.) lens_report lens_Report_reportTitle
          {-# INLINE lens_Report_reportTitle #-}
          lens_Report_reportUUID :: forall . Lens' c UUID
          lens_Report_reportUUID = (.) lens_report lens_Report_reportUUID
          {-# INLINE lens_Report_reportUUID #-}
          lens_Report_reportValueApproachInfo :: forall . Lens' c
                                                                ReportValueApproachInfo
          lens_Report_reportValueApproachInfo = (.) lens_report lens_Report_reportValueApproachInfo
          {-# INLINE lens_Report_reportValueApproachInfo #-}
          lens_Report_reportValueTypeInfo :: forall . Lens' c
                                                            ReportValueTypeInfo
          lens_Report_reportValueTypeInfo = (.) lens_report lens_Report_reportValueTypeInfo
          {-# INLINE lens_Report_reportValueTypeInfo #-}
class HasReportElem c
    where lens_reportElem :: Lens' c ReportElem
          lens_ReportElem_elemItem :: forall . Traversal' c Item
          lens_ReportElem_elemItem = (.) lens_reportElem lens_ReportElem_elemItem
          {-# INLINE lens_ReportElem_elemItem #-}
          lens_ReportElem_elemText :: forall . Traversal' c Markup
          lens_ReportElem_elemText = (.) lens_reportElem lens_ReportElem_elemText
          {-# INLINE lens_ReportElem_elemText #-}
class HasReportFlags c
    where lens_reportFlags :: Lens' c ReportFlags
          lens_ReportFlags_hideEmptyItemFields :: forall . Lens' c Bool
          lens_ReportFlags_hideEmptyItemFields = (.) lens_reportFlags lens_ReportFlags_hideEmptyItemFields
          {-# INLINE lens_ReportFlags_hideEmptyItemFields #-}
class HasReportImage c
    where lens_reportImage :: Lens' c ReportImage
          lens_ReportImage_picCaption :: forall . Lens' c Markup
          lens_ReportImage_picCaption = (.) lens_reportImage lens_ReportImage_picCaption
          {-# INLINE lens_ReportImage_picCaption #-}
          lens_ReportImage_picCrop :: forall . Lens' c ImageCrop
          lens_ReportImage_picCrop = (.) lens_reportImage lens_ReportImage_picCrop
          {-# INLINE lens_ReportImage_picCrop #-}
          lens_ReportImage_picEditedDeprecated :: forall . Lens' c
                                                                 MaybeImageFile
          lens_ReportImage_picEditedDeprecated = (.) lens_reportImage lens_ReportImage_picEditedDeprecated
          {-# INLINE lens_ReportImage_picEditedDeprecated #-}
          lens_ReportImage_picEnlargedDeprecated :: forall . Lens' c
                                                                   MaybeImageFile
          lens_ReportImage_picEnlargedDeprecated = (.) lens_reportImage lens_ReportImage_picEnlargedDeprecated
          {-# INLINE lens_ReportImage_picEnlargedDeprecated #-}
          lens_ReportImage_picMustEnlarge :: forall . Lens' c Bool
          lens_ReportImage_picMustEnlarge = (.) lens_reportImage lens_ReportImage_picMustEnlarge
          {-# INLINE lens_ReportImage_picMustEnlarge #-}
          lens_ReportImage_picOriginal :: forall . Lens' c
                                                         (Maybe (Either URI ImageFile))
          lens_ReportImage_picOriginal = (.) lens_reportImage lens_ReportImage_picOriginal
          {-# INLINE lens_ReportImage_picOriginal #-}
          lens_ReportImage_picPrinterDeprecated :: forall . Lens' c
                                                                  MaybeImageFile
          lens_ReportImage_picPrinterDeprecated = (.) lens_reportImage lens_ReportImage_picPrinterDeprecated
          {-# INLINE lens_ReportImage_picPrinterDeprecated #-}
          lens_ReportImage_picSize :: forall . Lens' c ImageSize
          lens_ReportImage_picSize = (.) lens_reportImage lens_ReportImage_picSize
          {-# INLINE lens_ReportImage_picSize #-}
          lens_ReportImage_picThumbDeprecated :: forall . Lens' c
                                                                MaybeImageFile
          lens_ReportImage_picThumbDeprecated = (.) lens_reportImage lens_ReportImage_picThumbDeprecated
          {-# INLINE lens_ReportImage_picThumbDeprecated #-}
class HasReportImageView c
    where lens_reportImageView :: Lens' c ReportImageView
          lens_ReportImageView__picCaption :: forall . Lens' c Markup
          lens_ReportImageView__picCaption = (.) lens_reportImageView lens_ReportImageView__picCaption
          {-# INLINE lens_ReportImageView__picCaption #-}
          lens_ReportImageView__picCrop :: forall . Lens' c ImageCrop
          lens_ReportImageView__picCrop = (.) lens_reportImageView lens_ReportImageView__picCrop
          {-# INLINE lens_ReportImageView__picCrop #-}
          lens_ReportImageView__picEditedDeprecated :: forall . Lens' c
                                                                      MaybeImageFile
          lens_ReportImageView__picEditedDeprecated = (.) lens_reportImageView lens_ReportImageView__picEditedDeprecated
          {-# INLINE lens_ReportImageView__picEditedDeprecated #-}
          lens_ReportImageView__picEnlargedDeprecated :: forall . Lens' c
                                                                        MaybeImageFile
          lens_ReportImageView__picEnlargedDeprecated = (.) lens_reportImageView lens_ReportImageView__picEnlargedDeprecated
          {-# INLINE lens_ReportImageView__picEnlargedDeprecated #-}
          lens_ReportImageView__picMustEnlarge :: forall . Lens' c Bool
          lens_ReportImageView__picMustEnlarge = (.) lens_reportImageView lens_ReportImageView__picMustEnlarge
          {-# INLINE lens_ReportImageView__picMustEnlarge #-}
          lens_ReportImageView__picOriginal :: forall . Lens' c
                                                              (Maybe (Either URI ImageFile))
          lens_ReportImageView__picOriginal = (.) lens_reportImageView lens_ReportImageView__picOriginal
          {-# INLINE lens_ReportImageView__picOriginal #-}
          lens_ReportImageView__picPrinterDeprecated :: forall . Lens' c
                                                                       MaybeImageFile
          lens_ReportImageView__picPrinterDeprecated = (.) lens_reportImageView lens_ReportImageView__picPrinterDeprecated
          {-# INLINE lens_ReportImageView__picPrinterDeprecated #-}
          lens_ReportImageView__picSize :: forall . Lens' c SaneSizeImageSize
          lens_ReportImageView__picSize = (.) lens_reportImageView lens_ReportImageView__picSize
          {-# INLINE lens_ReportImageView__picSize #-}
          lens_ReportImageView__picThumbDeprecated :: forall . Lens' c
                                                                     MaybeImageFile
          lens_ReportImageView__picThumbDeprecated = (.) lens_reportImageView lens_ReportImageView__picThumbDeprecated
          {-# INLINE lens_ReportImageView__picThumbDeprecated #-}
class HasReportIntendedUse c
    where lens_reportIntendedUse :: Lens' c ReportIntendedUse
class HasReportMap c
    where reportMap :: Lens' c ReportMap
          lens_ReportMap_unReportMap :: forall . Lens' c
                                                       (Map ReportID Report)
          lens_ReportMap_unReportMap = (.) reportMap lens_ReportMap_unReportMap
          {-# INLINE lens_ReportMap_unReportMap #-}
class HasReportStatus c
    where lens_reportStatus :: Lens' c ReportStatus
class HasReportValueApproachInfo c
    where lens_reportValueApproachInfo :: Lens' c
                                                ReportValueApproachInfo
          lens_ReportValueApproachInfo_reportValueApproachDescription :: forall . Lens' c
                                                                                        Markup
          lens_ReportValueApproachInfo_reportValueApproachDescription = (.) lens_reportValueApproachInfo lens_ReportValueApproachInfo_reportValueApproachDescription
          {-# INLINE lens_ReportValueApproachInfo_reportValueApproachDescription #-}
          lens_ReportValueApproachInfo_reportValueApproachName :: forall . Lens' c
                                                                                 Markup
          lens_ReportValueApproachInfo_reportValueApproachName = (.) lens_reportValueApproachInfo lens_ReportValueApproachInfo_reportValueApproachName
          {-# INLINE lens_ReportValueApproachInfo_reportValueApproachName #-}
class HasReportValueTypeInfo c
    where lens_reportValueTypeInfo :: Lens' c ReportValueTypeInfo
          lens_ReportValueTypeInfo_reportValueTypeDefinition :: forall . Lens' c
                                                                               Markup
          lens_ReportValueTypeInfo_reportValueTypeDefinition = (.) lens_reportValueTypeInfo lens_ReportValueTypeInfo_reportValueTypeDefinition
          {-# INLINE lens_ReportValueTypeInfo_reportValueTypeDefinition #-}
          lens_ReportValueTypeInfo_reportValueTypeDescription :: forall . Lens' c
                                                                                Markup
          lens_ReportValueTypeInfo_reportValueTypeDescription = (.) lens_reportValueTypeInfo lens_ReportValueTypeInfo_reportValueTypeDescription
          {-# INLINE lens_ReportValueTypeInfo_reportValueTypeDescription #-}
          lens_ReportValueTypeInfo_reportValueTypeName :: forall . Lens' c
                                                                         Markup
          lens_ReportValueTypeInfo_reportValueTypeName = (.) lens_reportValueTypeInfo lens_ReportValueTypeInfo_reportValueTypeName
          {-# INLINE lens_ReportValueTypeInfo_reportValueTypeName #-}
class HasReportView c
    where lens_reportView :: Lens' c ReportView
          lens_ReportView__reportAbbrevs :: forall . Lens' c AbbrevPairs
          lens_ReportView__reportAbbrevs = (.) lens_reportView lens_ReportView__reportAbbrevs
          {-# INLINE lens_ReportView__reportAbbrevs #-}
          lens_ReportView__reportAuthors :: forall . Lens' c Authors
          lens_ReportView__reportAuthors = (.) lens_reportView lens_ReportView__reportAuthors
          {-# INLINE lens_ReportView__reportAuthors #-}
          lens_ReportView__reportBody :: forall . Lens' c ReportElems
          lens_ReportView__reportBody = (.) lens_reportView lens_ReportView__reportBody
          {-# INLINE lens_ReportView__reportBody #-}
          lens_ReportView__reportBranding :: forall . Lens' c Branding
          lens_ReportView__reportBranding = (.) lens_reportView lens_ReportView__reportBranding
          {-# INLINE lens_ReportView__reportBranding #-}
          lens_ReportView__reportBriefItems :: forall . Lens' c Markup
          lens_ReportView__reportBriefItems = (.) lens_reportView lens_ReportView__reportBriefItems
          {-# INLINE lens_ReportView__reportBriefItems #-}
          lens_ReportView__reportCertification :: forall . Lens' c Markups
          lens_ReportView__reportCertification = (.) lens_reportView lens_ReportView__reportCertification
          {-# INLINE lens_ReportView__reportCertification #-}
          lens_ReportView__reportClientAddress :: forall . Lens' c Markup
          lens_ReportView__reportClientAddress = (.) lens_reportView lens_ReportView__reportClientAddress
          {-# INLINE lens_ReportView__reportClientAddress #-}
          lens_ReportView__reportClientGreeting :: forall . Lens' c Markup
          lens_ReportView__reportClientGreeting = (.) lens_reportView lens_ReportView__reportClientGreeting
          {-# INLINE lens_ReportView__reportClientGreeting #-}
          lens_ReportView__reportClientName :: forall . Lens' c Markup
          lens_ReportView__reportClientName = (.) lens_reportView lens_ReportView__reportClientName
          {-# INLINE lens_ReportView__reportClientName #-}
          lens_ReportView__reportContractDate :: forall . Lens' c Markup
          lens_ReportView__reportContractDate = (.) lens_reportView lens_ReportView__reportContractDate
          {-# INLINE lens_ReportView__reportContractDate #-}
          lens_ReportView__reportCreated :: forall . Lens' c EpochMilli
          lens_ReportView__reportCreated = (.) lens_reportView lens_ReportView__reportCreated
          {-# INLINE lens_ReportView__reportCreated #-}
          lens_ReportView__reportDate :: forall . Lens' c Markup
          lens_ReportView__reportDate = (.) lens_reportView lens_ReportView__reportDate
          {-# INLINE lens_ReportView__reportDate #-}
          lens_ReportView__reportDisplayItemName :: forall . Lens' c Bool
          lens_ReportView__reportDisplayItemName = (.) lens_reportView lens_ReportView__reportDisplayItemName
          {-# INLINE lens_ReportView__reportDisplayItemName #-}
          lens_ReportView__reportEffectiveDate :: forall . Lens' c Markup
          lens_ReportView__reportEffectiveDate = (.) lens_reportView lens_ReportView__reportEffectiveDate
          {-# INLINE lens_ReportView__reportEffectiveDate #-}
          lens_ReportView__reportFlags :: forall . Lens' c ReportFlags
          lens_ReportView__reportFlags = (.) lens_reportView lens_ReportView__reportFlags
          {-# INLINE lens_ReportView__reportFlags #-}
          lens_ReportView__reportFolder :: forall . Lens' c ReadOnlyFilePath
          lens_ReportView__reportFolder = (.) lens_reportView lens_ReportView__reportFolder
          {-# INLINE lens_ReportView__reportFolder #-}
          lens_ReportView__reportFooter :: forall . Lens' c Markup
          lens_ReportView__reportFooter = (.) lens_reportView lens_ReportView__reportFooter
          {-# INLINE lens_ReportView__reportFooter #-}
          lens_ReportView__reportGlossary :: forall . Lens' c MarkupPairs
          lens_ReportView__reportGlossary = (.) lens_reportView lens_ReportView__reportGlossary
          {-# INLINE lens_ReportView__reportGlossary #-}
          lens_ReportView__reportHeader :: forall . Lens' c Markup
          lens_ReportView__reportHeader = (.) lens_reportView lens_ReportView__reportHeader
          {-# INLINE lens_ReportView__reportHeader #-}
          lens_ReportView__reportInspectionDate :: forall . Lens' c Markup
          lens_ReportView__reportInspectionDate = (.) lens_reportView lens_ReportView__reportInspectionDate
          {-# INLINE lens_ReportView__reportInspectionDate #-}
          lens_ReportView__reportInspectionLocation :: forall . Lens' c
                                                                      Markup
          lens_ReportView__reportInspectionLocation = (.) lens_reportView lens_ReportView__reportInspectionLocation
          {-# INLINE lens_ReportView__reportInspectionLocation #-}
          lens_ReportView__reportIntendedUse :: forall . Lens' c
                                                               MaybeReportIntendedUse
          lens_ReportView__reportIntendedUse = (.) lens_reportView lens_ReportView__reportIntendedUse
          {-# INLINE lens_ReportView__reportIntendedUse #-}
          lens_ReportView__reportItemsOwner :: forall . Lens' c Markup
          lens_ReportView__reportItemsOwner = (.) lens_reportView lens_ReportView__reportItemsOwner
          {-# INLINE lens_ReportView__reportItemsOwner #-}
          lens_ReportView__reportItemsOwnerFull :: forall . Lens' c Markup
          lens_ReportView__reportItemsOwnerFull = (.) lens_reportView lens_ReportView__reportItemsOwnerFull
          {-# INLINE lens_ReportView__reportItemsOwnerFull #-}
          lens_ReportView__reportLetterOfTransmittal :: forall . Lens' c
                                                                       Markup
          lens_ReportView__reportLetterOfTransmittal = (.) lens_reportView lens_ReportView__reportLetterOfTransmittal
          {-# INLINE lens_ReportView__reportLetterOfTransmittal #-}
          lens_ReportView__reportLimitingConditions :: forall . Lens' c
                                                                      Markups
          lens_ReportView__reportLimitingConditions = (.) lens_reportView lens_ReportView__reportLimitingConditions
          {-# INLINE lens_ReportView__reportLimitingConditions #-}
          lens_ReportView__reportName :: forall . Lens' c Markup
          lens_ReportView__reportName = (.) lens_reportView lens_ReportView__reportName
          {-# INLINE lens_ReportView__reportName #-}
          lens_ReportView__reportOrderByItemName :: forall . Lens' c Bool
          lens_ReportView__reportOrderByItemName = (.) lens_reportView lens_ReportView__reportOrderByItemName
          {-# INLINE lens_ReportView__reportOrderByItemName #-}
          lens_ReportView__reportPerms :: forall . Lens' c Permissions
          lens_ReportView__reportPerms = (.) lens_reportView lens_ReportView__reportPerms
          {-# INLINE lens_ReportView__reportPerms #-}
          lens_ReportView__reportPreparer :: forall . Lens' c Markup
          lens_ReportView__reportPreparer = (.) lens_reportView lens_ReportView__reportPreparer
          {-# INLINE lens_ReportView__reportPreparer #-}
          lens_ReportView__reportPreparerAddress :: forall . Lens' c Markup
          lens_ReportView__reportPreparerAddress = (.) lens_reportView lens_ReportView__reportPreparerAddress
          {-# INLINE lens_ReportView__reportPreparerAddress #-}
          lens_ReportView__reportPreparerEIN :: forall . Lens' c Markup
          lens_ReportView__reportPreparerEIN = (.) lens_reportView lens_ReportView__reportPreparerEIN
          {-# INLINE lens_ReportView__reportPreparerEIN #-}
          lens_ReportView__reportPreparerEMail :: forall . Lens' c Markup
          lens_ReportView__reportPreparerEMail = (.) lens_reportView lens_ReportView__reportPreparerEMail
          {-# INLINE lens_ReportView__reportPreparerEMail #-}
          lens_ReportView__reportPreparerWebsite :: forall . Lens' c Markup
          lens_ReportView__reportPreparerWebsite = (.) lens_reportView lens_ReportView__reportPreparerWebsite
          {-# INLINE lens_ReportView__reportPreparerWebsite #-}
          lens_ReportView__reportPrivacyPolicy :: forall . Lens' c Markup
          lens_ReportView__reportPrivacyPolicy = (.) lens_reportView lens_ReportView__reportPrivacyPolicy
          {-# INLINE lens_ReportView__reportPrivacyPolicy #-}
          lens_ReportView__reportRedacted :: forall . Lens' c Bool
          lens_ReportView__reportRedacted = (.) lens_reportView lens_ReportView__reportRedacted
          {-# INLINE lens_ReportView__reportRedacted #-}
          lens_ReportView__reportRevision :: forall . Lens' c Integer
          lens_ReportView__reportRevision = (.) lens_reportView lens_ReportView__reportRevision
          {-# INLINE lens_ReportView__reportRevision #-}
          lens_ReportView__reportScopeOfWork :: forall . Lens' c Markup
          lens_ReportView__reportScopeOfWork = (.) lens_reportView lens_ReportView__reportScopeOfWork
          {-# INLINE lens_ReportView__reportScopeOfWork #-}
          lens_ReportView__reportSources :: forall . Lens' c MarkupPairs
          lens_ReportView__reportSources = (.) lens_reportView lens_ReportView__reportSources
          {-# INLINE lens_ReportView__reportSources #-}
          lens_ReportView__reportStatus :: forall . Lens' c ReportStatus
          lens_ReportView__reportStatus = (.) lens_reportView lens_ReportView__reportStatus
          {-# INLINE lens_ReportView__reportStatus #-}
          lens_ReportView__reportTitle :: forall . Lens' c Markup
          lens_ReportView__reportTitle = (.) lens_reportView lens_ReportView__reportTitle
          {-# INLINE lens_ReportView__reportTitle #-}
          lens_ReportView__reportUUID :: forall . Lens' c UUID
          lens_ReportView__reportUUID = (.) lens_reportView lens_ReportView__reportUUID
          {-# INLINE lens_ReportView__reportUUID #-}
          lens_ReportView__reportValueApproachInfo :: forall . Lens' c
                                                                     ReportValueApproachInfo
          lens_ReportView__reportValueApproachInfo = (.) lens_reportView lens_ReportView__reportValueApproachInfo
          {-# INLINE lens_ReportView__reportValueApproachInfo #-}
          lens_ReportView__reportValueTypeInfo :: forall . Lens' c
                                                                 ReportValueTypeInfo
          lens_ReportView__reportValueTypeInfo = (.) lens_reportView lens_ReportView__reportValueTypeInfo
          {-# INLINE lens_ReportView__reportValueTypeInfo #-}
class HasText c
    where lens_text :: Lens' c Text
class HasUnits c
    where lens_units :: Lens' c Units
instance HasAuthor Author
    where lens_author = id
          lens_Author_authorCredentials f (Author x1
                                                  x2) = fmap (\y1 -> Author x1 y1) (f x2)
          {-# INLINE lens_Author_authorCredentials #-}
          lens_Author_authorName f (Author x1
                                           x2) = fmap (\y1 -> Author y1 x2) (f x1)
          {-# INLINE lens_Author_authorName #-}
instance HasBool Bool
    where lens_bool = id
instance HasBranding Branding
    where lens_branding = id
instance HasCIString CIString
    where cIString = id
          lens_CIString_unCIString = iso (\(CIString x) -> x) CIString
          {-# INLINE lens_CIString_unCIString #-}
instance HasDimension Dimension
    where lens_dimension = id
instance HasDouble Double
    where lens_double = id
instance HasImageSize ImageSize
    where lens_imageSize = id
          lens_ImageSize_dim f (ImageSize x1
                                          x2
                                          x3) = fmap (\y1 -> ImageSize y1 x2 x3) (f x1)
          {-# INLINE lens_ImageSize_dim #-}
          lens_ImageSize_size f (ImageSize x1
                                           x2
                                           x3) = fmap (\y1 -> ImageSize x1 y1 x3) (f x2)
          {-# INLINE lens_ImageSize_size #-}
          lens_ImageSize_units f (ImageSize x1
                                            x2
                                            x3) = fmap (\y1 -> ImageSize x1 x2 y1) (f x3)
          {-# INLINE lens_ImageSize_units #-}
instance HasItem Item
    where lens_item = id
          lens_Item_fields f (Item x1
                                   x2
                                   x3) = fmap (\y1 -> Item x1 y1 x3) (f x2)
          {-# INLINE lens_Item_fields #-}
          lens_Item_images f (Item x1
                                   x2
                                   x3) = fmap (\y1 -> Item x1 x2 y1) (f x3)
          {-# INLINE lens_Item_images #-}
          lens_Item_itemName f (Item x1
                                     x2
                                     x3) = fmap (\y1 -> Item y1 x2 x3) (f x1)
          {-# INLINE lens_Item_itemName #-}
instance HasMarkup Markup
    where lens_markup = id
          lens_Markup_htmlText _ (Markdown x1) = pure (Markdown x1)
          lens_Markup_htmlText f (Html x1) = fmap (\y1 -> Html y1) (f x1)
          lens_Markup_htmlText _ (LaTeX x1) = pure (LaTeX x1)
          lens_Markup_htmlText _ (Pandoc x1) = pure (Pandoc x1)
          lens_Markup_htmlText _ (Markup x1) = pure (Markup x1)
          {-# INLINE lens_Markup_htmlText #-}
          lens_Markup_markdownText f (Markdown x1) = fmap (\y1 -> Markdown y1) (f x1)
          lens_Markup_markdownText _ (Html x1) = pure (Html x1)
          lens_Markup_markdownText _ (LaTeX x1) = pure (LaTeX x1)
          lens_Markup_markdownText _ (Pandoc x1) = pure (Pandoc x1)
          lens_Markup_markdownText _ (Markup x1) = pure (Markup x1)
          {-# INLINE lens_Markup_markdownText #-}
instance HasPermissions Permissions
    where lens_permissions = id
          lens_Permissions_owner f (Permissions x1
                                                x2
                                                x3) = fmap (\y1 -> Permissions y1 x2 x3) (f x1)
          {-# INLINE lens_Permissions_owner #-}
          lens_Permissions_readers f (Permissions x1
                                                  x2
                                                  x3) = fmap (\y1 -> Permissions x1 x2 y1) (f x3)
          {-# INLINE lens_Permissions_readers #-}
          lens_Permissions_writers f (Permissions x1
                                                  x2
                                                  x3) = fmap (\y1 -> Permissions x1 y1 x3) (f x2)
          {-# INLINE lens_Permissions_writers #-}
instance HasReport Report
    where lens_report = id
          lens_Report_reportAbbrevs f (Report x1
                                              x2
                                              x3
                                              x4
                                              x5
                                              x6
                                              x7
                                              x8
                                              x9
                                              x10
                                              x11
                                              x12
                                              x13
                                              x14
                                              x15
                                              x16
                                              x17
                                              x18
                                              x19
                                              x20
                                              x21
                                              x22
                                              x23
                                              x24
                                              x25
                                              x26
                                              x27
                                              x28
                                              x29
                                              x30
                                              x31
                                              x32
                                              x33
                                              x34
                                              x35
                                              x36
                                              x37
                                              x38
                                              x39
                                              x40
                                              x41
                                              x42
                                              x43
                                              x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 y1 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x13)
          {-# INLINE lens_Report_reportAbbrevs #-}
          lens_Report_reportAuthors f (Report x1
                                              x2
                                              x3
                                              x4
                                              x5
                                              x6
                                              x7
                                              x8
                                              x9
                                              x10
                                              x11
                                              x12
                                              x13
                                              x14
                                              x15
                                              x16
                                              x17
                                              x18
                                              x19
                                              x20
                                              x21
                                              x22
                                              x23
                                              x24
                                              x25
                                              x26
                                              x27
                                              x28
                                              x29
                                              x30
                                              x31
                                              x32
                                              x33
                                              x34
                                              x35
                                              x36
                                              x37
                                              x38
                                              x39
                                              x40
                                              x41
                                              x42
                                              x43
                                              x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 y1 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x7)
          {-# INLINE lens_Report_reportAuthors #-}
          lens_Report_reportBody f (Report x1
                                           x2
                                           x3
                                           x4
                                           x5
                                           x6
                                           x7
                                           x8
                                           x9
                                           x10
                                           x11
                                           x12
                                           x13
                                           x14
                                           x15
                                           x16
                                           x17
                                           x18
                                           x19
                                           x20
                                           x21
                                           x22
                                           x23
                                           x24
                                           x25
                                           x26
                                           x27
                                           x28
                                           x29
                                           x30
                                           x31
                                           x32
                                           x33
                                           x34
                                           x35
                                           x36
                                           x37
                                           x38
                                           x39
                                           x40
                                           x41
                                           x42
                                           x43
                                           x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 y1 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x27)
          {-# INLINE lens_Report_reportBody #-}
          lens_Report_reportBranding f (Report x1
                                               x2
                                               x3
                                               x4
                                               x5
                                               x6
                                               x7
                                               x8
                                               x9
                                               x10
                                               x11
                                               x12
                                               x13
                                               x14
                                               x15
                                               x16
                                               x17
                                               x18
                                               x19
                                               x20
                                               x21
                                               x22
                                               x23
                                               x24
                                               x25
                                               x26
                                               x27
                                               x28
                                               x29
                                               x30
                                               x31
                                               x32
                                               x33
                                               x34
                                               x35
                                               x36
                                               x37
                                               x38
                                               x39
                                               x40
                                               x41
                                               x42
                                               x43
                                               x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 y1 x39 x40 x41 x42 x43 x44) (f x38)
          {-# INLINE lens_Report_reportBranding #-}
          lens_Report_reportBriefItems f (Report x1
                                                 x2
                                                 x3
                                                 x4
                                                 x5
                                                 x6
                                                 x7
                                                 x8
                                                 x9
                                                 x10
                                                 x11
                                                 x12
                                                 x13
                                                 x14
                                                 x15
                                                 x16
                                                 x17
                                                 x18
                                                 x19
                                                 x20
                                                 x21
                                                 x22
                                                 x23
                                                 x24
                                                 x25
                                                 x26
                                                 x27
                                                 x28
                                                 x29
                                                 x30
                                                 x31
                                                 x32
                                                 x33
                                                 x34
                                                 x35
                                                 x36
                                                 x37
                                                 x38
                                                 x39
                                                 x40
                                                 x41
                                                 x42
                                                 x43
                                                 x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 y1 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x25)
          {-# INLINE lens_Report_reportBriefItems #-}
          lens_Report_reportCertification f (Report x1
                                                    x2
                                                    x3
                                                    x4
                                                    x5
                                                    x6
                                                    x7
                                                    x8
                                                    x9
                                                    x10
                                                    x11
                                                    x12
                                                    x13
                                                    x14
                                                    x15
                                                    x16
                                                    x17
                                                    x18
                                                    x19
                                                    x20
                                                    x21
                                                    x22
                                                    x23
                                                    x24
                                                    x25
                                                    x26
                                                    x27
                                                    x28
                                                    x29
                                                    x30
                                                    x31
                                                    x32
                                                    x33
                                                    x34
                                                    x35
                                                    x36
                                                    x37
                                                    x38
                                                    x39
                                                    x40
                                                    x41
                                                    x42
                                                    x43
                                                    x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 y1 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x32)
          {-# INLINE lens_Report_reportCertification #-}
          lens_Report_reportClientAddress f (Report x1
                                                    x2
                                                    x3
                                                    x4
                                                    x5
                                                    x6
                                                    x7
                                                    x8
                                                    x9
                                                    x10
                                                    x11
                                                    x12
                                                    x13
                                                    x14
                                                    x15
                                                    x16
                                                    x17
                                                    x18
                                                    x19
                                                    x20
                                                    x21
                                                    x22
                                                    x23
                                                    x24
                                                    x25
                                                    x26
                                                    x27
                                                    x28
                                                    x29
                                                    x30
                                                    x31
                                                    x32
                                                    x33
                                                    x34
                                                    x35
                                                    x36
                                                    x37
                                                    x38
                                                    x39
                                                    x40
                                                    x41
                                                    x42
                                                    x43
                                                    x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 y1 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x21)
          {-# INLINE lens_Report_reportClientAddress #-}
          lens_Report_reportClientGreeting f (Report x1
                                                     x2
                                                     x3
                                                     x4
                                                     x5
                                                     x6
                                                     x7
                                                     x8
                                                     x9
                                                     x10
                                                     x11
                                                     x12
                                                     x13
                                                     x14
                                                     x15
                                                     x16
                                                     x17
                                                     x18
                                                     x19
                                                     x20
                                                     x21
                                                     x22
                                                     x23
                                                     x24
                                                     x25
                                                     x26
                                                     x27
                                                     x28
                                                     x29
                                                     x30
                                                     x31
                                                     x32
                                                     x33
                                                     x34
                                                     x35
                                                     x36
                                                     x37
                                                     x38
                                                     x39
                                                     x40
                                                     x41
                                                     x42
                                                     x43
                                                     x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 y1 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x22)
          {-# INLINE lens_Report_reportClientGreeting #-}
          lens_Report_reportClientName f (Report x1
                                                 x2
                                                 x3
                                                 x4
                                                 x5
                                                 x6
                                                 x7
                                                 x8
                                                 x9
                                                 x10
                                                 x11
                                                 x12
                                                 x13
                                                 x14
                                                 x15
                                                 x16
                                                 x17
                                                 x18
                                                 x19
                                                 x20
                                                 x21
                                                 x22
                                                 x23
                                                 x24
                                                 x25
                                                 x26
                                                 x27
                                                 x28
                                                 x29
                                                 x30
                                                 x31
                                                 x32
                                                 x33
                                                 x34
                                                 x35
                                                 x36
                                                 x37
                                                 x38
                                                 x39
                                                 x40
                                                 x41
                                                 x42
                                                 x43
                                                 x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 y1 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x20)
          {-# INLINE lens_Report_reportClientName #-}
          lens_Report_reportContractDate f (Report x1
                                                   x2
                                                   x3
                                                   x4
                                                   x5
                                                   x6
                                                   x7
                                                   x8
                                                   x9
                                                   x10
                                                   x11
                                                   x12
                                                   x13
                                                   x14
                                                   x15
                                                   x16
                                                   x17
                                                   x18
                                                   x19
                                                   x20
                                                   x21
                                                   x22
                                                   x23
                                                   x24
                                                   x25
                                                   x26
                                                   x27
                                                   x28
                                                   x29
                                                   x30
                                                   x31
                                                   x32
                                                   x33
                                                   x34
                                                   x35
                                                   x36
                                                   x37
                                                   x38
                                                   x39
                                                   x40
                                                   x41
                                                   x42
                                                   x43
                                                   x44) = fmap (\y1 -> Report x1 x2 x3 y1 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x4)
          {-# INLINE lens_Report_reportContractDate #-}
          lens_Report_reportCreated f (Report x1
                                              x2
                                              x3
                                              x4
                                              x5
                                              x6
                                              x7
                                              x8
                                              x9
                                              x10
                                              x11
                                              x12
                                              x13
                                              x14
                                              x15
                                              x16
                                              x17
                                              x18
                                              x19
                                              x20
                                              x21
                                              x22
                                              x23
                                              x24
                                              x25
                                              x26
                                              x27
                                              x28
                                              x29
                                              x30
                                              x31
                                              x32
                                              x33
                                              x34
                                              x35
                                              x36
                                              x37
                                              x38
                                              x39
                                              x40
                                              x41
                                              x42
                                              x43
                                              x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 y1 x38 x39 x40 x41 x42 x43 x44) (f x37)
          {-# INLINE lens_Report_reportCreated #-}
          lens_Report_reportDate f (Report x1
                                           x2
                                           x3
                                           x4
                                           x5
                                           x6
                                           x7
                                           x8
                                           x9
                                           x10
                                           x11
                                           x12
                                           x13
                                           x14
                                           x15
                                           x16
                                           x17
                                           x18
                                           x19
                                           x20
                                           x21
                                           x22
                                           x23
                                           x24
                                           x25
                                           x26
                                           x27
                                           x28
                                           x29
                                           x30
                                           x31
                                           x32
                                           x33
                                           x34
                                           x35
                                           x36
                                           x37
                                           x38
                                           x39
                                           x40
                                           x41
                                           x42
                                           x43
                                           x44) = fmap (\y1 -> Report x1 x2 y1 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x3)
          {-# INLINE lens_Report_reportDate #-}
          lens_Report_reportDisplayItemName f (Report x1
                                                      x2
                                                      x3
                                                      x4
                                                      x5
                                                      x6
                                                      x7
                                                      x8
                                                      x9
                                                      x10
                                                      x11
                                                      x12
                                                      x13
                                                      x14
                                                      x15
                                                      x16
                                                      x17
                                                      x18
                                                      x19
                                                      x20
                                                      x21
                                                      x22
                                                      x23
                                                      x24
                                                      x25
                                                      x26
                                                      x27
                                                      x28
                                                      x29
                                                      x30
                                                      x31
                                                      x32
                                                      x33
                                                      x34
                                                      x35
                                                      x36
                                                      x37
                                                      x38
                                                      x39
                                                      x40
                                                      x41
                                                      x42
                                                      x43
                                                      x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 y1) (f x44)
          {-# INLINE lens_Report_reportDisplayItemName #-}
          lens_Report_reportEffectiveDate f (Report x1
                                                    x2
                                                    x3
                                                    x4
                                                    x5
                                                    x6
                                                    x7
                                                    x8
                                                    x9
                                                    x10
                                                    x11
                                                    x12
                                                    x13
                                                    x14
                                                    x15
                                                    x16
                                                    x17
                                                    x18
                                                    x19
                                                    x20
                                                    x21
                                                    x22
                                                    x23
                                                    x24
                                                    x25
                                                    x26
                                                    x27
                                                    x28
                                                    x29
                                                    x30
                                                    x31
                                                    x32
                                                    x33
                                                    x34
                                                    x35
                                                    x36
                                                    x37
                                                    x38
                                                    x39
                                                    x40
                                                    x41
                                                    x42
                                                    x43
                                                    x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 y1 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x6)
          {-# INLINE lens_Report_reportEffectiveDate #-}
          lens_Report_reportFlags f (Report x1
                                            x2
                                            x3
                                            x4
                                            x5
                                            x6
                                            x7
                                            x8
                                            x9
                                            x10
                                            x11
                                            x12
                                            x13
                                            x14
                                            x15
                                            x16
                                            x17
                                            x18
                                            x19
                                            x20
                                            x21
                                            x22
                                            x23
                                            x24
                                            x25
                                            x26
                                            x27
                                            x28
                                            x29
                                            x30
                                            x31
                                            x32
                                            x33
                                            x34
                                            x35
                                            x36
                                            x37
                                            x38
                                            x39
                                            x40
                                            x41
                                            x42
                                            x43
                                            x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 y1 x42 x43 x44) (f x41)
          {-# INLINE lens_Report_reportFlags #-}
          lens_Report_reportFolder f (Report x1
                                             x2
                                             x3
                                             x4
                                             x5
                                             x6
                                             x7
                                             x8
                                             x9
                                             x10
                                             x11
                                             x12
                                             x13
                                             x14
                                             x15
                                             x16
                                             x17
                                             x18
                                             x19
                                             x20
                                             x21
                                             x22
                                             x23
                                             x24
                                             x25
                                             x26
                                             x27
                                             x28
                                             x29
                                             x30
                                             x31
                                             x32
                                             x33
                                             x34
                                             x35
                                             x36
                                             x37
                                             x38
                                             x39
                                             x40
                                             x41
                                             x42
                                             x43
                                             x44) = fmap (\y1 -> Report y1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x1)
          {-# INLINE lens_Report_reportFolder #-}
          lens_Report_reportFooter f (Report x1
                                             x2
                                             x3
                                             x4
                                             x5
                                             x6
                                             x7
                                             x8
                                             x9
                                             x10
                                             x11
                                             x12
                                             x13
                                             x14
                                             x15
                                             x16
                                             x17
                                             x18
                                             x19
                                             x20
                                             x21
                                             x22
                                             x23
                                             x24
                                             x25
                                             x26
                                             x27
                                             x28
                                             x29
                                             x30
                                             x31
                                             x32
                                             x33
                                             x34
                                             x35
                                             x36
                                             x37
                                             x38
                                             x39
                                             x40
                                             x41
                                             x42
                                             x43
                                             x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 y1 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x16)
          {-# INLINE lens_Report_reportFooter #-}
          lens_Report_reportGlossary f (Report x1
                                               x2
                                               x3
                                               x4
                                               x5
                                               x6
                                               x7
                                               x8
                                               x9
                                               x10
                                               x11
                                               x12
                                               x13
                                               x14
                                               x15
                                               x16
                                               x17
                                               x18
                                               x19
                                               x20
                                               x21
                                               x22
                                               x23
                                               x24
                                               x25
                                               x26
                                               x27
                                               x28
                                               x29
                                               x30
                                               x31
                                               x32
                                               x33
                                               x34
                                               x35
                                               x36
                                               x37
                                               x38
                                               x39
                                               x40
                                               x41
                                               x42
                                               x43
                                               x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 y1 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x28)
          {-# INLINE lens_Report_reportGlossary #-}
          lens_Report_reportHeader f (Report x1
                                             x2
                                             x3
                                             x4
                                             x5
                                             x6
                                             x7
                                             x8
                                             x9
                                             x10
                                             x11
                                             x12
                                             x13
                                             x14
                                             x15
                                             x16
                                             x17
                                             x18
                                             x19
                                             x20
                                             x21
                                             x22
                                             x23
                                             x24
                                             x25
                                             x26
                                             x27
                                             x28
                                             x29
                                             x30
                                             x31
                                             x32
                                             x33
                                             x34
                                             x35
                                             x36
                                             x37
                                             x38
                                             x39
                                             x40
                                             x41
                                             x42
                                             x43
                                             x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 y1 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x15)
          {-# INLINE lens_Report_reportHeader #-}
          lens_Report_reportInspectionDate f (Report x1
                                                     x2
                                                     x3
                                                     x4
                                                     x5
                                                     x6
                                                     x7
                                                     x8
                                                     x9
                                                     x10
                                                     x11
                                                     x12
                                                     x13
                                                     x14
                                                     x15
                                                     x16
                                                     x17
                                                     x18
                                                     x19
                                                     x20
                                                     x21
                                                     x22
                                                     x23
                                                     x24
                                                     x25
                                                     x26
                                                     x27
                                                     x28
                                                     x29
                                                     x30
                                                     x31
                                                     x32
                                                     x33
                                                     x34
                                                     x35
                                                     x36
                                                     x37
                                                     x38
                                                     x39
                                                     x40
                                                     x41
                                                     x42
                                                     x43
                                                     x44) = fmap (\y1 -> Report x1 x2 x3 x4 y1 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x5)
          {-# INLINE lens_Report_reportInspectionDate #-}
          lens_Report_reportInspectionLocation f (Report x1
                                                         x2
                                                         x3
                                                         x4
                                                         x5
                                                         x6
                                                         x7
                                                         x8
                                                         x9
                                                         x10
                                                         x11
                                                         x12
                                                         x13
                                                         x14
                                                         x15
                                                         x16
                                                         x17
                                                         x18
                                                         x19
                                                         x20
                                                         x21
                                                         x22
                                                         x23
                                                         x24
                                                         x25
                                                         x26
                                                         x27
                                                         x28
                                                         x29
                                                         x30
                                                         x31
                                                         x32
                                                         x33
                                                         x34
                                                         x35
                                                         x36
                                                         x37
                                                         x38
                                                         x39
                                                         x40
                                                         x41
                                                         x42
                                                         x43
                                                         x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 y1 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x26)
          {-# INLINE lens_Report_reportInspectionLocation #-}
          lens_Report_reportIntendedUse f (Report x1
                                                  x2
                                                  x3
                                                  x4
                                                  x5
                                                  x6
                                                  x7
                                                  x8
                                                  x9
                                                  x10
                                                  x11
                                                  x12
                                                  x13
                                                  x14
                                                  x15
                                                  x16
                                                  x17
                                                  x18
                                                  x19
                                                  x20
                                                  x21
                                                  x22
                                                  x23
                                                  x24
                                                  x25
                                                  x26
                                                  x27
                                                  x28
                                                  x29
                                                  x30
                                                  x31
                                                  x32
                                                  x33
                                                  x34
                                                  x35
                                                  x36
                                                  x37
                                                  x38
                                                  x39
                                                  x40
                                                  x41
                                                  x42
                                                  x43
                                                  x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 y1 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x17)
          {-# INLINE lens_Report_reportIntendedUse #-}
          lens_Report_reportItemsOwner f (Report x1
                                                 x2
                                                 x3
                                                 x4
                                                 x5
                                                 x6
                                                 x7
                                                 x8
                                                 x9
                                                 x10
                                                 x11
                                                 x12
                                                 x13
                                                 x14
                                                 x15
                                                 x16
                                                 x17
                                                 x18
                                                 x19
                                                 x20
                                                 x21
                                                 x22
                                                 x23
                                                 x24
                                                 x25
                                                 x26
                                                 x27
                                                 x28
                                                 x29
                                                 x30
                                                 x31
                                                 x32
                                                 x33
                                                 x34
                                                 x35
                                                 x36
                                                 x37
                                                 x38
                                                 x39
                                                 x40
                                                 x41
                                                 x42
                                                 x43
                                                 x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 y1 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x24)
          {-# INLINE lens_Report_reportItemsOwner #-}
          lens_Report_reportItemsOwnerFull f (Report x1
                                                     x2
                                                     x3
                                                     x4
                                                     x5
                                                     x6
                                                     x7
                                                     x8
                                                     x9
                                                     x10
                                                     x11
                                                     x12
                                                     x13
                                                     x14
                                                     x15
                                                     x16
                                                     x17
                                                     x18
                                                     x19
                                                     x20
                                                     x21
                                                     x22
                                                     x23
                                                     x24
                                                     x25
                                                     x26
                                                     x27
                                                     x28
                                                     x29
                                                     x30
                                                     x31
                                                     x32
                                                     x33
                                                     x34
                                                     x35
                                                     x36
                                                     x37
                                                     x38
                                                     x39
                                                     x40
                                                     x41
                                                     x42
                                                     x43
                                                     x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 y1 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x23)
          {-# INLINE lens_Report_reportItemsOwnerFull #-}
          lens_Report_reportLetterOfTransmittal f (Report x1
                                                          x2
                                                          x3
                                                          x4
                                                          x5
                                                          x6
                                                          x7
                                                          x8
                                                          x9
                                                          x10
                                                          x11
                                                          x12
                                                          x13
                                                          x14
                                                          x15
                                                          x16
                                                          x17
                                                          x18
                                                          x19
                                                          x20
                                                          x21
                                                          x22
                                                          x23
                                                          x24
                                                          x25
                                                          x26
                                                          x27
                                                          x28
                                                          x29
                                                          x30
                                                          x31
                                                          x32
                                                          x33
                                                          x34
                                                          x35
                                                          x36
                                                          x37
                                                          x38
                                                          x39
                                                          x40
                                                          x41
                                                          x42
                                                          x43
                                                          x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 y1 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x30)
          {-# INLINE lens_Report_reportLetterOfTransmittal #-}
          lens_Report_reportLimitingConditions f (Report x1
                                                         x2
                                                         x3
                                                         x4
                                                         x5
                                                         x6
                                                         x7
                                                         x8
                                                         x9
                                                         x10
                                                         x11
                                                         x12
                                                         x13
                                                         x14
                                                         x15
                                                         x16
                                                         x17
                                                         x18
                                                         x19
                                                         x20
                                                         x21
                                                         x22
                                                         x23
                                                         x24
                                                         x25
                                                         x26
                                                         x27
                                                         x28
                                                         x29
                                                         x30
                                                         x31
                                                         x32
                                                         x33
                                                         x34
                                                         x35
                                                         x36
                                                         x37
                                                         x38
                                                         x39
                                                         x40
                                                         x41
                                                         x42
                                                         x43
                                                         x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 y1 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x33)
          {-# INLINE lens_Report_reportLimitingConditions #-}
          lens_Report_reportName f (Report x1
                                           x2
                                           x3
                                           x4
                                           x5
                                           x6
                                           x7
                                           x8
                                           x9
                                           x10
                                           x11
                                           x12
                                           x13
                                           x14
                                           x15
                                           x16
                                           x17
                                           x18
                                           x19
                                           x20
                                           x21
                                           x22
                                           x23
                                           x24
                                           x25
                                           x26
                                           x27
                                           x28
                                           x29
                                           x30
                                           x31
                                           x32
                                           x33
                                           x34
                                           x35
                                           x36
                                           x37
                                           x38
                                           x39
                                           x40
                                           x41
                                           x42
                                           x43
                                           x44) = fmap (\y1 -> Report x1 y1 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x2)
          {-# INLINE lens_Report_reportName #-}
          lens_Report_reportOrderByItemName f (Report x1
                                                      x2
                                                      x3
                                                      x4
                                                      x5
                                                      x6
                                                      x7
                                                      x8
                                                      x9
                                                      x10
                                                      x11
                                                      x12
                                                      x13
                                                      x14
                                                      x15
                                                      x16
                                                      x17
                                                      x18
                                                      x19
                                                      x20
                                                      x21
                                                      x22
                                                      x23
                                                      x24
                                                      x25
                                                      x26
                                                      x27
                                                      x28
                                                      x29
                                                      x30
                                                      x31
                                                      x32
                                                      x33
                                                      x34
                                                      x35
                                                      x36
                                                      x37
                                                      x38
                                                      x39
                                                      x40
                                                      x41
                                                      x42
                                                      x43
                                                      x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 y1 x44) (f x43)
          {-# INLINE lens_Report_reportOrderByItemName #-}
          lens_Report_reportPerms f (Report x1
                                            x2
                                            x3
                                            x4
                                            x5
                                            x6
                                            x7
                                            x8
                                            x9
                                            x10
                                            x11
                                            x12
                                            x13
                                            x14
                                            x15
                                            x16
                                            x17
                                            x18
                                            x19
                                            x20
                                            x21
                                            x22
                                            x23
                                            x24
                                            x25
                                            x26
                                            x27
                                            x28
                                            x29
                                            x30
                                            x31
                                            x32
                                            x33
                                            x34
                                            x35
                                            x36
                                            x37
                                            x38
                                            x39
                                            x40
                                            x41
                                            x42
                                            x43
                                            x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 y1 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x35)
          {-# INLINE lens_Report_reportPerms #-}
          lens_Report_reportPreparer f (Report x1
                                               x2
                                               x3
                                               x4
                                               x5
                                               x6
                                               x7
                                               x8
                                               x9
                                               x10
                                               x11
                                               x12
                                               x13
                                               x14
                                               x15
                                               x16
                                               x17
                                               x18
                                               x19
                                               x20
                                               x21
                                               x22
                                               x23
                                               x24
                                               x25
                                               x26
                                               x27
                                               x28
                                               x29
                                               x30
                                               x31
                                               x32
                                               x33
                                               x34
                                               x35
                                               x36
                                               x37
                                               x38
                                               x39
                                               x40
                                               x41
                                               x42
                                               x43
                                               x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 y1 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x8)
          {-# INLINE lens_Report_reportPreparer #-}
          lens_Report_reportPreparerAddress f (Report x1
                                                      x2
                                                      x3
                                                      x4
                                                      x5
                                                      x6
                                                      x7
                                                      x8
                                                      x9
                                                      x10
                                                      x11
                                                      x12
                                                      x13
                                                      x14
                                                      x15
                                                      x16
                                                      x17
                                                      x18
                                                      x19
                                                      x20
                                                      x21
                                                      x22
                                                      x23
                                                      x24
                                                      x25
                                                      x26
                                                      x27
                                                      x28
                                                      x29
                                                      x30
                                                      x31
                                                      x32
                                                      x33
                                                      x34
                                                      x35
                                                      x36
                                                      x37
                                                      x38
                                                      x39
                                                      x40
                                                      x41
                                                      x42
                                                      x43
                                                      x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 y1 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x10)
          {-# INLINE lens_Report_reportPreparerAddress #-}
          lens_Report_reportPreparerEIN f (Report x1
                                                  x2
                                                  x3
                                                  x4
                                                  x5
                                                  x6
                                                  x7
                                                  x8
                                                  x9
                                                  x10
                                                  x11
                                                  x12
                                                  x13
                                                  x14
                                                  x15
                                                  x16
                                                  x17
                                                  x18
                                                  x19
                                                  x20
                                                  x21
                                                  x22
                                                  x23
                                                  x24
                                                  x25
                                                  x26
                                                  x27
                                                  x28
                                                  x29
                                                  x30
                                                  x31
                                                  x32
                                                  x33
                                                  x34
                                                  x35
                                                  x36
                                                  x37
                                                  x38
                                                  x39
                                                  x40
                                                  x41
                                                  x42
                                                  x43
                                                  x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 y1 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x9)
          {-# INLINE lens_Report_reportPreparerEIN #-}
          lens_Report_reportPreparerEMail f (Report x1
                                                    x2
                                                    x3
                                                    x4
                                                    x5
                                                    x6
                                                    x7
                                                    x8
                                                    x9
                                                    x10
                                                    x11
                                                    x12
                                                    x13
                                                    x14
                                                    x15
                                                    x16
                                                    x17
                                                    x18
                                                    x19
                                                    x20
                                                    x21
                                                    x22
                                                    x23
                                                    x24
                                                    x25
                                                    x26
                                                    x27
                                                    x28
                                                    x29
                                                    x30
                                                    x31
                                                    x32
                                                    x33
                                                    x34
                                                    x35
                                                    x36
                                                    x37
                                                    x38
                                                    x39
                                                    x40
                                                    x41
                                                    x42
                                                    x43
                                                    x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 y1 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x11)
          {-# INLINE lens_Report_reportPreparerEMail #-}
          lens_Report_reportPreparerWebsite f (Report x1
                                                      x2
                                                      x3
                                                      x4
                                                      x5
                                                      x6
                                                      x7
                                                      x8
                                                      x9
                                                      x10
                                                      x11
                                                      x12
                                                      x13
                                                      x14
                                                      x15
                                                      x16
                                                      x17
                                                      x18
                                                      x19
                                                      x20
                                                      x21
                                                      x22
                                                      x23
                                                      x24
                                                      x25
                                                      x26
                                                      x27
                                                      x28
                                                      x29
                                                      x30
                                                      x31
                                                      x32
                                                      x33
                                                      x34
                                                      x35
                                                      x36
                                                      x37
                                                      x38
                                                      x39
                                                      x40
                                                      x41
                                                      x42
                                                      x43
                                                      x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 y1 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x12)
          {-# INLINE lens_Report_reportPreparerWebsite #-}
          lens_Report_reportPrivacyPolicy f (Report x1
                                                    x2
                                                    x3
                                                    x4
                                                    x5
                                                    x6
                                                    x7
                                                    x8
                                                    x9
                                                    x10
                                                    x11
                                                    x12
                                                    x13
                                                    x14
                                                    x15
                                                    x16
                                                    x17
                                                    x18
                                                    x19
                                                    x20
                                                    x21
                                                    x22
                                                    x23
                                                    x24
                                                    x25
                                                    x26
                                                    x27
                                                    x28
                                                    x29
                                                    x30
                                                    x31
                                                    x32
                                                    x33
                                                    x34
                                                    x35
                                                    x36
                                                    x37
                                                    x38
                                                    x39
                                                    x40
                                                    x41
                                                    x42
                                                    x43
                                                    x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 y1 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x34)
          {-# INLINE lens_Report_reportPrivacyPolicy #-}
          lens_Report_reportRedacted f (Report x1
                                               x2
                                               x3
                                               x4
                                               x5
                                               x6
                                               x7
                                               x8
                                               x9
                                               x10
                                               x11
                                               x12
                                               x13
                                               x14
                                               x15
                                               x16
                                               x17
                                               x18
                                               x19
                                               x20
                                               x21
                                               x22
                                               x23
                                               x24
                                               x25
                                               x26
                                               x27
                                               x28
                                               x29
                                               x30
                                               x31
                                               x32
                                               x33
                                               x34
                                               x35
                                               x36
                                               x37
                                               x38
                                               x39
                                               x40
                                               x41
                                               x42
                                               x43
                                               x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 y1 x41 x42 x43 x44) (f x40)
          {-# INLINE lens_Report_reportRedacted #-}
          lens_Report_reportRevision f (Report x1
                                               x2
                                               x3
                                               x4
                                               x5
                                               x6
                                               x7
                                               x8
                                               x9
                                               x10
                                               x11
                                               x12
                                               x13
                                               x14
                                               x15
                                               x16
                                               x17
                                               x18
                                               x19
                                               x20
                                               x21
                                               x22
                                               x23
                                               x24
                                               x25
                                               x26
                                               x27
                                               x28
                                               x29
                                               x30
                                               x31
                                               x32
                                               x33
                                               x34
                                               x35
                                               x36
                                               x37
                                               x38
                                               x39
                                               x40
                                               x41
                                               x42
                                               x43
                                               x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 y1 x37 x38 x39 x40 x41 x42 x43 x44) (f x36)
          {-# INLINE lens_Report_reportRevision #-}
          lens_Report_reportScopeOfWork f (Report x1
                                                  x2
                                                  x3
                                                  x4
                                                  x5
                                                  x6
                                                  x7
                                                  x8
                                                  x9
                                                  x10
                                                  x11
                                                  x12
                                                  x13
                                                  x14
                                                  x15
                                                  x16
                                                  x17
                                                  x18
                                                  x19
                                                  x20
                                                  x21
                                                  x22
                                                  x23
                                                  x24
                                                  x25
                                                  x26
                                                  x27
                                                  x28
                                                  x29
                                                  x30
                                                  x31
                                                  x32
                                                  x33
                                                  x34
                                                  x35
                                                  x36
                                                  x37
                                                  x38
                                                  x39
                                                  x40
                                                  x41
                                                  x42
                                                  x43
                                                  x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 y1 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x31)
          {-# INLINE lens_Report_reportScopeOfWork #-}
          lens_Report_reportSources f (Report x1
                                              x2
                                              x3
                                              x4
                                              x5
                                              x6
                                              x7
                                              x8
                                              x9
                                              x10
                                              x11
                                              x12
                                              x13
                                              x14
                                              x15
                                              x16
                                              x17
                                              x18
                                              x19
                                              x20
                                              x21
                                              x22
                                              x23
                                              x24
                                              x25
                                              x26
                                              x27
                                              x28
                                              x29
                                              x30
                                              x31
                                              x32
                                              x33
                                              x34
                                              x35
                                              x36
                                              x37
                                              x38
                                              x39
                                              x40
                                              x41
                                              x42
                                              x43
                                              x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 y1 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x29)
          {-# INLINE lens_Report_reportSources #-}
          lens_Report_reportStatus f (Report x1
                                             x2
                                             x3
                                             x4
                                             x5
                                             x6
                                             x7
                                             x8
                                             x9
                                             x10
                                             x11
                                             x12
                                             x13
                                             x14
                                             x15
                                             x16
                                             x17
                                             x18
                                             x19
                                             x20
                                             x21
                                             x22
                                             x23
                                             x24
                                             x25
                                             x26
                                             x27
                                             x28
                                             x29
                                             x30
                                             x31
                                             x32
                                             x33
                                             x34
                                             x35
                                             x36
                                             x37
                                             x38
                                             x39
                                             x40
                                             x41
                                             x42
                                             x43
                                             x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 y1 x40 x41 x42 x43 x44) (f x39)
          {-# INLINE lens_Report_reportStatus #-}
          lens_Report_reportTitle f (Report x1
                                            x2
                                            x3
                                            x4
                                            x5
                                            x6
                                            x7
                                            x8
                                            x9
                                            x10
                                            x11
                                            x12
                                            x13
                                            x14
                                            x15
                                            x16
                                            x17
                                            x18
                                            x19
                                            x20
                                            x21
                                            x22
                                            x23
                                            x24
                                            x25
                                            x26
                                            x27
                                            x28
                                            x29
                                            x30
                                            x31
                                            x32
                                            x33
                                            x34
                                            x35
                                            x36
                                            x37
                                            x38
                                            x39
                                            x40
                                            x41
                                            x42
                                            x43
                                            x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 y1 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x14)
          {-# INLINE lens_Report_reportTitle #-}
          lens_Report_reportUUID f (Report x1
                                           x2
                                           x3
                                           x4
                                           x5
                                           x6
                                           x7
                                           x8
                                           x9
                                           x10
                                           x11
                                           x12
                                           x13
                                           x14
                                           x15
                                           x16
                                           x17
                                           x18
                                           x19
                                           x20
                                           x21
                                           x22
                                           x23
                                           x24
                                           x25
                                           x26
                                           x27
                                           x28
                                           x29
                                           x30
                                           x31
                                           x32
                                           x33
                                           x34
                                           x35
                                           x36
                                           x37
                                           x38
                                           x39
                                           x40
                                           x41
                                           x42
                                           x43
                                           x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 y1 x43 x44) (f x42)
          {-# INLINE lens_Report_reportUUID #-}
          lens_Report_reportValueApproachInfo f (Report x1
                                                        x2
                                                        x3
                                                        x4
                                                        x5
                                                        x6
                                                        x7
                                                        x8
                                                        x9
                                                        x10
                                                        x11
                                                        x12
                                                        x13
                                                        x14
                                                        x15
                                                        x16
                                                        x17
                                                        x18
                                                        x19
                                                        x20
                                                        x21
                                                        x22
                                                        x23
                                                        x24
                                                        x25
                                                        x26
                                                        x27
                                                        x28
                                                        x29
                                                        x30
                                                        x31
                                                        x32
                                                        x33
                                                        x34
                                                        x35
                                                        x36
                                                        x37
                                                        x38
                                                        x39
                                                        x40
                                                        x41
                                                        x42
                                                        x43
                                                        x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 y1 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x19)
          {-# INLINE lens_Report_reportValueApproachInfo #-}
          lens_Report_reportValueTypeInfo f (Report x1
                                                    x2
                                                    x3
                                                    x4
                                                    x5
                                                    x6
                                                    x7
                                                    x8
                                                    x9
                                                    x10
                                                    x11
                                                    x12
                                                    x13
                                                    x14
                                                    x15
                                                    x16
                                                    x17
                                                    x18
                                                    x19
                                                    x20
                                                    x21
                                                    x22
                                                    x23
                                                    x24
                                                    x25
                                                    x26
                                                    x27
                                                    x28
                                                    x29
                                                    x30
                                                    x31
                                                    x32
                                                    x33
                                                    x34
                                                    x35
                                                    x36
                                                    x37
                                                    x38
                                                    x39
                                                    x40
                                                    x41
                                                    x42
                                                    x43
                                                    x44) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 y1 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x18)
          {-# INLINE lens_Report_reportValueTypeInfo #-}
instance HasReportElem ReportElem
    where lens_reportElem = id
          lens_ReportElem_elemItem f (ReportItem x1) = fmap (\y1 -> ReportItem y1) (f x1)
          lens_ReportElem_elemItem _ (ReportParagraph x1) = pure (ReportParagraph x1)
          lens_ReportElem_elemItem _ (ReportUndecided) = pure ReportUndecided
          {-# INLINE lens_ReportElem_elemItem #-}
          lens_ReportElem_elemText _ (ReportItem x1) = pure (ReportItem x1)
          lens_ReportElem_elemText f (ReportParagraph x1) = fmap (\y1 -> ReportParagraph y1) (f x1)
          lens_ReportElem_elemText _ (ReportUndecided) = pure ReportUndecided
          {-# INLINE lens_ReportElem_elemText #-}
instance HasReportFlags ReportFlags
    where lens_reportFlags = id
          lens_ReportFlags_hideEmptyItemFields = iso (\(ReportFlags x) -> x) ReportFlags
          {-# INLINE lens_ReportFlags_hideEmptyItemFields #-}
instance HasReportImage ReportImage
    where lens_reportImage = id
          lens_ReportImage_picCaption f (Pic x1
                                             x2
                                             x3
                                             x4
                                             x5
                                             x6
                                             x7
                                             x8
                                             x9) = fmap (\y1 -> Pic x1 x2 y1 x4 x5 x6 x7 x8 x9) (f x3)
          {-# INLINE lens_ReportImage_picCaption #-}
          lens_ReportImage_picCrop f (Pic x1
                                          x2
                                          x3
                                          x4
                                          x5
                                          x6
                                          x7
                                          x8
                                          x9) = fmap (\y1 -> Pic x1 y1 x3 x4 x5 x6 x7 x8 x9) (f x2)
          {-# INLINE lens_ReportImage_picCrop #-}
          lens_ReportImage_picEditedDeprecated f (Pic x1
                                                      x2
                                                      x3
                                                      x4
                                                      x5
                                                      x6
                                                      x7
                                                      x8
                                                      x9) = fmap (\y1 -> Pic x1 x2 x3 x4 y1 x6 x7 x8 x9) (f x5)
          {-# INLINE lens_ReportImage_picEditedDeprecated #-}
          lens_ReportImage_picEnlargedDeprecated f (Pic x1
                                                        x2
                                                        x3
                                                        x4
                                                        x5
                                                        x6
                                                        x7
                                                        x8
                                                        x9) = fmap (\y1 -> Pic x1 x2 x3 x4 x5 x6 x7 x8 y1) (f x9)
          {-# INLINE lens_ReportImage_picEnlargedDeprecated #-}
          lens_ReportImage_picMustEnlarge f (Pic x1
                                                 x2
                                                 x3
                                                 x4
                                                 x5
                                                 x6
                                                 x7
                                                 x8
                                                 x9) = fmap (\y1 -> Pic x1 x2 x3 x4 x5 x6 x7 y1 x9) (f x8)
          {-# INLINE lens_ReportImage_picMustEnlarge #-}
          lens_ReportImage_picOriginal f (Pic x1
                                              x2
                                              x3
                                              x4
                                              x5
                                              x6
                                              x7
                                              x8
                                              x9) = fmap (\y1 -> Pic x1 x2 x3 y1 x5 x6 x7 x8 x9) (f x4)
          {-# INLINE lens_ReportImage_picOriginal #-}
          lens_ReportImage_picPrinterDeprecated f (Pic x1
                                                       x2
                                                       x3
                                                       x4
                                                       x5
                                                       x6
                                                       x7
                                                       x8
                                                       x9) = fmap (\y1 -> Pic x1 x2 x3 x4 x5 x6 y1 x8 x9) (f x7)
          {-# INLINE lens_ReportImage_picPrinterDeprecated #-}
          lens_ReportImage_picSize f (Pic x1
                                          x2
                                          x3
                                          x4
                                          x5
                                          x6
                                          x7
                                          x8
                                          x9) = fmap (\y1 -> Pic y1 x2 x3 x4 x5 x6 x7 x8 x9) (f x1)
          {-# INLINE lens_ReportImage_picSize #-}
          lens_ReportImage_picThumbDeprecated f (Pic x1
                                                     x2
                                                     x3
                                                     x4
                                                     x5
                                                     x6
                                                     x7
                                                     x8
                                                     x9) = fmap (\y1 -> Pic x1 x2 x3 x4 x5 y1 x7 x8 x9) (f x6)
          {-# INLINE lens_ReportImage_picThumbDeprecated #-}
instance HasReportImageView ReportImageView
    where lens_reportImageView = id
          lens_ReportImageView__picCaption f (ReportImageView x1
                                                              x2
                                                              x3
                                                              x4
                                                              x5
                                                              x6
                                                              x7
                                                              x8
                                                              x9) = fmap (\y1 -> ReportImageView x1 x2 y1 x4 x5 x6 x7 x8 x9) (f x3)
          {-# INLINE lens_ReportImageView__picCaption #-}
          lens_ReportImageView__picCrop f (ReportImageView x1
                                                           x2
                                                           x3
                                                           x4
                                                           x5
                                                           x6
                                                           x7
                                                           x8
                                                           x9) = fmap (\y1 -> ReportImageView x1 y1 x3 x4 x5 x6 x7 x8 x9) (f x2)
          {-# INLINE lens_ReportImageView__picCrop #-}
          lens_ReportImageView__picEditedDeprecated f (ReportImageView x1
                                                                       x2
                                                                       x3
                                                                       x4
                                                                       x5
                                                                       x6
                                                                       x7
                                                                       x8
                                                                       x9) = fmap (\y1 -> ReportImageView x1 x2 x3 x4 y1 x6 x7 x8 x9) (f x5)
          {-# INLINE lens_ReportImageView__picEditedDeprecated #-}
          lens_ReportImageView__picEnlargedDeprecated f (ReportImageView x1
                                                                         x2
                                                                         x3
                                                                         x4
                                                                         x5
                                                                         x6
                                                                         x7
                                                                         x8
                                                                         x9) = fmap (\y1 -> ReportImageView x1 x2 x3 x4 x5 x6 x7 x8 y1) (f x9)
          {-# INLINE lens_ReportImageView__picEnlargedDeprecated #-}
          lens_ReportImageView__picMustEnlarge f (ReportImageView x1
                                                                  x2
                                                                  x3
                                                                  x4
                                                                  x5
                                                                  x6
                                                                  x7
                                                                  x8
                                                                  x9) = fmap (\y1 -> ReportImageView x1 x2 x3 x4 x5 x6 x7 y1 x9) (f x8)
          {-# INLINE lens_ReportImageView__picMustEnlarge #-}
          lens_ReportImageView__picOriginal f (ReportImageView x1
                                                               x2
                                                               x3
                                                               x4
                                                               x5
                                                               x6
                                                               x7
                                                               x8
                                                               x9) = fmap (\y1 -> ReportImageView x1 x2 x3 y1 x5 x6 x7 x8 x9) (f x4)
          {-# INLINE lens_ReportImageView__picOriginal #-}
          lens_ReportImageView__picPrinterDeprecated f (ReportImageView x1
                                                                        x2
                                                                        x3
                                                                        x4
                                                                        x5
                                                                        x6
                                                                        x7
                                                                        x8
                                                                        x9) = fmap (\y1 -> ReportImageView x1 x2 x3 x4 x5 x6 y1 x8 x9) (f x7)
          {-# INLINE lens_ReportImageView__picPrinterDeprecated #-}
          lens_ReportImageView__picSize f (ReportImageView x1
                                                           x2
                                                           x3
                                                           x4
                                                           x5
                                                           x6
                                                           x7
                                                           x8
                                                           x9) = fmap (\y1 -> ReportImageView y1 x2 x3 x4 x5 x6 x7 x8 x9) (f x1)
          {-# INLINE lens_ReportImageView__picSize #-}
          lens_ReportImageView__picThumbDeprecated f (ReportImageView x1
                                                                      x2
                                                                      x3
                                                                      x4
                                                                      x5
                                                                      x6
                                                                      x7
                                                                      x8
                                                                      x9) = fmap (\y1 -> ReportImageView x1 x2 x3 x4 x5 y1 x7 x8 x9) (f x6)
          {-# INLINE lens_ReportImageView__picThumbDeprecated #-}
instance HasReportIntendedUse ReportIntendedUse
    where lens_reportIntendedUse = id
instance HasReportMap ReportMap
    where reportMap = id
          lens_ReportMap_unReportMap = iso (\(ReportMap x) -> x) ReportMap
          {-# INLINE lens_ReportMap_unReportMap #-}
instance HasReportStatus ReportStatus
    where lens_reportStatus = id
instance HasReportValueApproachInfo ReportValueApproachInfo
    where lens_reportValueApproachInfo = id
          lens_ReportValueApproachInfo_reportValueApproachDescription f (ReportValueApproachInfo x1
                                                                                                 x2) = fmap (\y1 -> ReportValueApproachInfo x1 y1) (f x2)
          {-# INLINE lens_ReportValueApproachInfo_reportValueApproachDescription #-}
          lens_ReportValueApproachInfo_reportValueApproachName f (ReportValueApproachInfo x1
                                                                                          x2) = fmap (\y1 -> ReportValueApproachInfo y1 x2) (f x1)
          {-# INLINE lens_ReportValueApproachInfo_reportValueApproachName #-}
instance HasReportValueTypeInfo ReportValueTypeInfo
    where lens_reportValueTypeInfo = id
          lens_ReportValueTypeInfo_reportValueTypeDefinition f (ReportValueTypeInfo x1
                                                                                    x2
                                                                                    x3) = fmap (\y1 -> ReportValueTypeInfo x1 x2 y1) (f x3)
          {-# INLINE lens_ReportValueTypeInfo_reportValueTypeDefinition #-}
          lens_ReportValueTypeInfo_reportValueTypeDescription f (ReportValueTypeInfo x1
                                                                                     x2
                                                                                     x3) = fmap (\y1 -> ReportValueTypeInfo x1 y1 x3) (f x2)
          {-# INLINE lens_ReportValueTypeInfo_reportValueTypeDescription #-}
          lens_ReportValueTypeInfo_reportValueTypeName f (ReportValueTypeInfo x1
                                                                              x2
                                                                              x3) = fmap (\y1 -> ReportValueTypeInfo y1 x2 x3) (f x1)
          {-# INLINE lens_ReportValueTypeInfo_reportValueTypeName #-}
instance HasReportView ReportView
    where lens_reportView = id
          lens_ReportView__reportAbbrevs f (ReportView x1
                                                       x2
                                                       x3
                                                       x4
                                                       x5
                                                       x6
                                                       x7
                                                       x8
                                                       x9
                                                       x10
                                                       x11
                                                       x12
                                                       x13
                                                       x14
                                                       x15
                                                       x16
                                                       x17
                                                       x18
                                                       x19
                                                       x20
                                                       x21
                                                       x22
                                                       x23
                                                       x24
                                                       x25
                                                       x26
                                                       x27
                                                       x28
                                                       x29
                                                       x30
                                                       x31
                                                       x32
                                                       x33
                                                       x34
                                                       x35
                                                       x36
                                                       x37
                                                       x38
                                                       x39
                                                       x40
                                                       x41
                                                       x42
                                                       x43
                                                       x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 y1 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x13)
          {-# INLINE lens_ReportView__reportAbbrevs #-}
          lens_ReportView__reportAuthors f (ReportView x1
                                                       x2
                                                       x3
                                                       x4
                                                       x5
                                                       x6
                                                       x7
                                                       x8
                                                       x9
                                                       x10
                                                       x11
                                                       x12
                                                       x13
                                                       x14
                                                       x15
                                                       x16
                                                       x17
                                                       x18
                                                       x19
                                                       x20
                                                       x21
                                                       x22
                                                       x23
                                                       x24
                                                       x25
                                                       x26
                                                       x27
                                                       x28
                                                       x29
                                                       x30
                                                       x31
                                                       x32
                                                       x33
                                                       x34
                                                       x35
                                                       x36
                                                       x37
                                                       x38
                                                       x39
                                                       x40
                                                       x41
                                                       x42
                                                       x43
                                                       x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 y1 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x7)
          {-# INLINE lens_ReportView__reportAuthors #-}
          lens_ReportView__reportBody f (ReportView x1
                                                    x2
                                                    x3
                                                    x4
                                                    x5
                                                    x6
                                                    x7
                                                    x8
                                                    x9
                                                    x10
                                                    x11
                                                    x12
                                                    x13
                                                    x14
                                                    x15
                                                    x16
                                                    x17
                                                    x18
                                                    x19
                                                    x20
                                                    x21
                                                    x22
                                                    x23
                                                    x24
                                                    x25
                                                    x26
                                                    x27
                                                    x28
                                                    x29
                                                    x30
                                                    x31
                                                    x32
                                                    x33
                                                    x34
                                                    x35
                                                    x36
                                                    x37
                                                    x38
                                                    x39
                                                    x40
                                                    x41
                                                    x42
                                                    x43
                                                    x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 y1 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x27)
          {-# INLINE lens_ReportView__reportBody #-}
          lens_ReportView__reportBranding f (ReportView x1
                                                        x2
                                                        x3
                                                        x4
                                                        x5
                                                        x6
                                                        x7
                                                        x8
                                                        x9
                                                        x10
                                                        x11
                                                        x12
                                                        x13
                                                        x14
                                                        x15
                                                        x16
                                                        x17
                                                        x18
                                                        x19
                                                        x20
                                                        x21
                                                        x22
                                                        x23
                                                        x24
                                                        x25
                                                        x26
                                                        x27
                                                        x28
                                                        x29
                                                        x30
                                                        x31
                                                        x32
                                                        x33
                                                        x34
                                                        x35
                                                        x36
                                                        x37
                                                        x38
                                                        x39
                                                        x40
                                                        x41
                                                        x42
                                                        x43
                                                        x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 y1 x39 x40 x41 x42 x43 x44) (f x38)
          {-# INLINE lens_ReportView__reportBranding #-}
          lens_ReportView__reportBriefItems f (ReportView x1
                                                          x2
                                                          x3
                                                          x4
                                                          x5
                                                          x6
                                                          x7
                                                          x8
                                                          x9
                                                          x10
                                                          x11
                                                          x12
                                                          x13
                                                          x14
                                                          x15
                                                          x16
                                                          x17
                                                          x18
                                                          x19
                                                          x20
                                                          x21
                                                          x22
                                                          x23
                                                          x24
                                                          x25
                                                          x26
                                                          x27
                                                          x28
                                                          x29
                                                          x30
                                                          x31
                                                          x32
                                                          x33
                                                          x34
                                                          x35
                                                          x36
                                                          x37
                                                          x38
                                                          x39
                                                          x40
                                                          x41
                                                          x42
                                                          x43
                                                          x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 y1 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x25)
          {-# INLINE lens_ReportView__reportBriefItems #-}
          lens_ReportView__reportCertification f (ReportView x1
                                                             x2
                                                             x3
                                                             x4
                                                             x5
                                                             x6
                                                             x7
                                                             x8
                                                             x9
                                                             x10
                                                             x11
                                                             x12
                                                             x13
                                                             x14
                                                             x15
                                                             x16
                                                             x17
                                                             x18
                                                             x19
                                                             x20
                                                             x21
                                                             x22
                                                             x23
                                                             x24
                                                             x25
                                                             x26
                                                             x27
                                                             x28
                                                             x29
                                                             x30
                                                             x31
                                                             x32
                                                             x33
                                                             x34
                                                             x35
                                                             x36
                                                             x37
                                                             x38
                                                             x39
                                                             x40
                                                             x41
                                                             x42
                                                             x43
                                                             x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 y1 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x32)
          {-# INLINE lens_ReportView__reportCertification #-}
          lens_ReportView__reportClientAddress f (ReportView x1
                                                             x2
                                                             x3
                                                             x4
                                                             x5
                                                             x6
                                                             x7
                                                             x8
                                                             x9
                                                             x10
                                                             x11
                                                             x12
                                                             x13
                                                             x14
                                                             x15
                                                             x16
                                                             x17
                                                             x18
                                                             x19
                                                             x20
                                                             x21
                                                             x22
                                                             x23
                                                             x24
                                                             x25
                                                             x26
                                                             x27
                                                             x28
                                                             x29
                                                             x30
                                                             x31
                                                             x32
                                                             x33
                                                             x34
                                                             x35
                                                             x36
                                                             x37
                                                             x38
                                                             x39
                                                             x40
                                                             x41
                                                             x42
                                                             x43
                                                             x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 y1 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x21)
          {-# INLINE lens_ReportView__reportClientAddress #-}
          lens_ReportView__reportClientGreeting f (ReportView x1
                                                              x2
                                                              x3
                                                              x4
                                                              x5
                                                              x6
                                                              x7
                                                              x8
                                                              x9
                                                              x10
                                                              x11
                                                              x12
                                                              x13
                                                              x14
                                                              x15
                                                              x16
                                                              x17
                                                              x18
                                                              x19
                                                              x20
                                                              x21
                                                              x22
                                                              x23
                                                              x24
                                                              x25
                                                              x26
                                                              x27
                                                              x28
                                                              x29
                                                              x30
                                                              x31
                                                              x32
                                                              x33
                                                              x34
                                                              x35
                                                              x36
                                                              x37
                                                              x38
                                                              x39
                                                              x40
                                                              x41
                                                              x42
                                                              x43
                                                              x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 y1 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x22)
          {-# INLINE lens_ReportView__reportClientGreeting #-}
          lens_ReportView__reportClientName f (ReportView x1
                                                          x2
                                                          x3
                                                          x4
                                                          x5
                                                          x6
                                                          x7
                                                          x8
                                                          x9
                                                          x10
                                                          x11
                                                          x12
                                                          x13
                                                          x14
                                                          x15
                                                          x16
                                                          x17
                                                          x18
                                                          x19
                                                          x20
                                                          x21
                                                          x22
                                                          x23
                                                          x24
                                                          x25
                                                          x26
                                                          x27
                                                          x28
                                                          x29
                                                          x30
                                                          x31
                                                          x32
                                                          x33
                                                          x34
                                                          x35
                                                          x36
                                                          x37
                                                          x38
                                                          x39
                                                          x40
                                                          x41
                                                          x42
                                                          x43
                                                          x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 y1 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x20)
          {-# INLINE lens_ReportView__reportClientName #-}
          lens_ReportView__reportContractDate f (ReportView x1
                                                            x2
                                                            x3
                                                            x4
                                                            x5
                                                            x6
                                                            x7
                                                            x8
                                                            x9
                                                            x10
                                                            x11
                                                            x12
                                                            x13
                                                            x14
                                                            x15
                                                            x16
                                                            x17
                                                            x18
                                                            x19
                                                            x20
                                                            x21
                                                            x22
                                                            x23
                                                            x24
                                                            x25
                                                            x26
                                                            x27
                                                            x28
                                                            x29
                                                            x30
                                                            x31
                                                            x32
                                                            x33
                                                            x34
                                                            x35
                                                            x36
                                                            x37
                                                            x38
                                                            x39
                                                            x40
                                                            x41
                                                            x42
                                                            x43
                                                            x44) = fmap (\y1 -> ReportView x1 x2 x3 y1 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x4)
          {-# INLINE lens_ReportView__reportContractDate #-}
          lens_ReportView__reportCreated f (ReportView x1
                                                       x2
                                                       x3
                                                       x4
                                                       x5
                                                       x6
                                                       x7
                                                       x8
                                                       x9
                                                       x10
                                                       x11
                                                       x12
                                                       x13
                                                       x14
                                                       x15
                                                       x16
                                                       x17
                                                       x18
                                                       x19
                                                       x20
                                                       x21
                                                       x22
                                                       x23
                                                       x24
                                                       x25
                                                       x26
                                                       x27
                                                       x28
                                                       x29
                                                       x30
                                                       x31
                                                       x32
                                                       x33
                                                       x34
                                                       x35
                                                       x36
                                                       x37
                                                       x38
                                                       x39
                                                       x40
                                                       x41
                                                       x42
                                                       x43
                                                       x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 y1 x38 x39 x40 x41 x42 x43 x44) (f x37)
          {-# INLINE lens_ReportView__reportCreated #-}
          lens_ReportView__reportDate f (ReportView x1
                                                    x2
                                                    x3
                                                    x4
                                                    x5
                                                    x6
                                                    x7
                                                    x8
                                                    x9
                                                    x10
                                                    x11
                                                    x12
                                                    x13
                                                    x14
                                                    x15
                                                    x16
                                                    x17
                                                    x18
                                                    x19
                                                    x20
                                                    x21
                                                    x22
                                                    x23
                                                    x24
                                                    x25
                                                    x26
                                                    x27
                                                    x28
                                                    x29
                                                    x30
                                                    x31
                                                    x32
                                                    x33
                                                    x34
                                                    x35
                                                    x36
                                                    x37
                                                    x38
                                                    x39
                                                    x40
                                                    x41
                                                    x42
                                                    x43
                                                    x44) = fmap (\y1 -> ReportView x1 x2 y1 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x3)
          {-# INLINE lens_ReportView__reportDate #-}
          lens_ReportView__reportDisplayItemName f (ReportView x1
                                                               x2
                                                               x3
                                                               x4
                                                               x5
                                                               x6
                                                               x7
                                                               x8
                                                               x9
                                                               x10
                                                               x11
                                                               x12
                                                               x13
                                                               x14
                                                               x15
                                                               x16
                                                               x17
                                                               x18
                                                               x19
                                                               x20
                                                               x21
                                                               x22
                                                               x23
                                                               x24
                                                               x25
                                                               x26
                                                               x27
                                                               x28
                                                               x29
                                                               x30
                                                               x31
                                                               x32
                                                               x33
                                                               x34
                                                               x35
                                                               x36
                                                               x37
                                                               x38
                                                               x39
                                                               x40
                                                               x41
                                                               x42
                                                               x43
                                                               x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 y1) (f x44)
          {-# INLINE lens_ReportView__reportDisplayItemName #-}
          lens_ReportView__reportEffectiveDate f (ReportView x1
                                                             x2
                                                             x3
                                                             x4
                                                             x5
                                                             x6
                                                             x7
                                                             x8
                                                             x9
                                                             x10
                                                             x11
                                                             x12
                                                             x13
                                                             x14
                                                             x15
                                                             x16
                                                             x17
                                                             x18
                                                             x19
                                                             x20
                                                             x21
                                                             x22
                                                             x23
                                                             x24
                                                             x25
                                                             x26
                                                             x27
                                                             x28
                                                             x29
                                                             x30
                                                             x31
                                                             x32
                                                             x33
                                                             x34
                                                             x35
                                                             x36
                                                             x37
                                                             x38
                                                             x39
                                                             x40
                                                             x41
                                                             x42
                                                             x43
                                                             x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 y1 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x6)
          {-# INLINE lens_ReportView__reportEffectiveDate #-}
          lens_ReportView__reportFlags f (ReportView x1
                                                     x2
                                                     x3
                                                     x4
                                                     x5
                                                     x6
                                                     x7
                                                     x8
                                                     x9
                                                     x10
                                                     x11
                                                     x12
                                                     x13
                                                     x14
                                                     x15
                                                     x16
                                                     x17
                                                     x18
                                                     x19
                                                     x20
                                                     x21
                                                     x22
                                                     x23
                                                     x24
                                                     x25
                                                     x26
                                                     x27
                                                     x28
                                                     x29
                                                     x30
                                                     x31
                                                     x32
                                                     x33
                                                     x34
                                                     x35
                                                     x36
                                                     x37
                                                     x38
                                                     x39
                                                     x40
                                                     x41
                                                     x42
                                                     x43
                                                     x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 y1 x42 x43 x44) (f x41)
          {-# INLINE lens_ReportView__reportFlags #-}
          lens_ReportView__reportFolder f (ReportView x1
                                                      x2
                                                      x3
                                                      x4
                                                      x5
                                                      x6
                                                      x7
                                                      x8
                                                      x9
                                                      x10
                                                      x11
                                                      x12
                                                      x13
                                                      x14
                                                      x15
                                                      x16
                                                      x17
                                                      x18
                                                      x19
                                                      x20
                                                      x21
                                                      x22
                                                      x23
                                                      x24
                                                      x25
                                                      x26
                                                      x27
                                                      x28
                                                      x29
                                                      x30
                                                      x31
                                                      x32
                                                      x33
                                                      x34
                                                      x35
                                                      x36
                                                      x37
                                                      x38
                                                      x39
                                                      x40
                                                      x41
                                                      x42
                                                      x43
                                                      x44) = fmap (\y1 -> ReportView y1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x1)
          {-# INLINE lens_ReportView__reportFolder #-}
          lens_ReportView__reportFooter f (ReportView x1
                                                      x2
                                                      x3
                                                      x4
                                                      x5
                                                      x6
                                                      x7
                                                      x8
                                                      x9
                                                      x10
                                                      x11
                                                      x12
                                                      x13
                                                      x14
                                                      x15
                                                      x16
                                                      x17
                                                      x18
                                                      x19
                                                      x20
                                                      x21
                                                      x22
                                                      x23
                                                      x24
                                                      x25
                                                      x26
                                                      x27
                                                      x28
                                                      x29
                                                      x30
                                                      x31
                                                      x32
                                                      x33
                                                      x34
                                                      x35
                                                      x36
                                                      x37
                                                      x38
                                                      x39
                                                      x40
                                                      x41
                                                      x42
                                                      x43
                                                      x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 y1 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x16)
          {-# INLINE lens_ReportView__reportFooter #-}
          lens_ReportView__reportGlossary f (ReportView x1
                                                        x2
                                                        x3
                                                        x4
                                                        x5
                                                        x6
                                                        x7
                                                        x8
                                                        x9
                                                        x10
                                                        x11
                                                        x12
                                                        x13
                                                        x14
                                                        x15
                                                        x16
                                                        x17
                                                        x18
                                                        x19
                                                        x20
                                                        x21
                                                        x22
                                                        x23
                                                        x24
                                                        x25
                                                        x26
                                                        x27
                                                        x28
                                                        x29
                                                        x30
                                                        x31
                                                        x32
                                                        x33
                                                        x34
                                                        x35
                                                        x36
                                                        x37
                                                        x38
                                                        x39
                                                        x40
                                                        x41
                                                        x42
                                                        x43
                                                        x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 y1 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x28)
          {-# INLINE lens_ReportView__reportGlossary #-}
          lens_ReportView__reportHeader f (ReportView x1
                                                      x2
                                                      x3
                                                      x4
                                                      x5
                                                      x6
                                                      x7
                                                      x8
                                                      x9
                                                      x10
                                                      x11
                                                      x12
                                                      x13
                                                      x14
                                                      x15
                                                      x16
                                                      x17
                                                      x18
                                                      x19
                                                      x20
                                                      x21
                                                      x22
                                                      x23
                                                      x24
                                                      x25
                                                      x26
                                                      x27
                                                      x28
                                                      x29
                                                      x30
                                                      x31
                                                      x32
                                                      x33
                                                      x34
                                                      x35
                                                      x36
                                                      x37
                                                      x38
                                                      x39
                                                      x40
                                                      x41
                                                      x42
                                                      x43
                                                      x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 y1 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x15)
          {-# INLINE lens_ReportView__reportHeader #-}
          lens_ReportView__reportInspectionDate f (ReportView x1
                                                              x2
                                                              x3
                                                              x4
                                                              x5
                                                              x6
                                                              x7
                                                              x8
                                                              x9
                                                              x10
                                                              x11
                                                              x12
                                                              x13
                                                              x14
                                                              x15
                                                              x16
                                                              x17
                                                              x18
                                                              x19
                                                              x20
                                                              x21
                                                              x22
                                                              x23
                                                              x24
                                                              x25
                                                              x26
                                                              x27
                                                              x28
                                                              x29
                                                              x30
                                                              x31
                                                              x32
                                                              x33
                                                              x34
                                                              x35
                                                              x36
                                                              x37
                                                              x38
                                                              x39
                                                              x40
                                                              x41
                                                              x42
                                                              x43
                                                              x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 y1 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x5)
          {-# INLINE lens_ReportView__reportInspectionDate #-}
          lens_ReportView__reportInspectionLocation f (ReportView x1
                                                                  x2
                                                                  x3
                                                                  x4
                                                                  x5
                                                                  x6
                                                                  x7
                                                                  x8
                                                                  x9
                                                                  x10
                                                                  x11
                                                                  x12
                                                                  x13
                                                                  x14
                                                                  x15
                                                                  x16
                                                                  x17
                                                                  x18
                                                                  x19
                                                                  x20
                                                                  x21
                                                                  x22
                                                                  x23
                                                                  x24
                                                                  x25
                                                                  x26
                                                                  x27
                                                                  x28
                                                                  x29
                                                                  x30
                                                                  x31
                                                                  x32
                                                                  x33
                                                                  x34
                                                                  x35
                                                                  x36
                                                                  x37
                                                                  x38
                                                                  x39
                                                                  x40
                                                                  x41
                                                                  x42
                                                                  x43
                                                                  x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 y1 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x26)
          {-# INLINE lens_ReportView__reportInspectionLocation #-}
          lens_ReportView__reportIntendedUse f (ReportView x1
                                                           x2
                                                           x3
                                                           x4
                                                           x5
                                                           x6
                                                           x7
                                                           x8
                                                           x9
                                                           x10
                                                           x11
                                                           x12
                                                           x13
                                                           x14
                                                           x15
                                                           x16
                                                           x17
                                                           x18
                                                           x19
                                                           x20
                                                           x21
                                                           x22
                                                           x23
                                                           x24
                                                           x25
                                                           x26
                                                           x27
                                                           x28
                                                           x29
                                                           x30
                                                           x31
                                                           x32
                                                           x33
                                                           x34
                                                           x35
                                                           x36
                                                           x37
                                                           x38
                                                           x39
                                                           x40
                                                           x41
                                                           x42
                                                           x43
                                                           x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 y1 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x17)
          {-# INLINE lens_ReportView__reportIntendedUse #-}
          lens_ReportView__reportItemsOwner f (ReportView x1
                                                          x2
                                                          x3
                                                          x4
                                                          x5
                                                          x6
                                                          x7
                                                          x8
                                                          x9
                                                          x10
                                                          x11
                                                          x12
                                                          x13
                                                          x14
                                                          x15
                                                          x16
                                                          x17
                                                          x18
                                                          x19
                                                          x20
                                                          x21
                                                          x22
                                                          x23
                                                          x24
                                                          x25
                                                          x26
                                                          x27
                                                          x28
                                                          x29
                                                          x30
                                                          x31
                                                          x32
                                                          x33
                                                          x34
                                                          x35
                                                          x36
                                                          x37
                                                          x38
                                                          x39
                                                          x40
                                                          x41
                                                          x42
                                                          x43
                                                          x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 y1 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x24)
          {-# INLINE lens_ReportView__reportItemsOwner #-}
          lens_ReportView__reportItemsOwnerFull f (ReportView x1
                                                              x2
                                                              x3
                                                              x4
                                                              x5
                                                              x6
                                                              x7
                                                              x8
                                                              x9
                                                              x10
                                                              x11
                                                              x12
                                                              x13
                                                              x14
                                                              x15
                                                              x16
                                                              x17
                                                              x18
                                                              x19
                                                              x20
                                                              x21
                                                              x22
                                                              x23
                                                              x24
                                                              x25
                                                              x26
                                                              x27
                                                              x28
                                                              x29
                                                              x30
                                                              x31
                                                              x32
                                                              x33
                                                              x34
                                                              x35
                                                              x36
                                                              x37
                                                              x38
                                                              x39
                                                              x40
                                                              x41
                                                              x42
                                                              x43
                                                              x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 y1 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x23)
          {-# INLINE lens_ReportView__reportItemsOwnerFull #-}
          lens_ReportView__reportLetterOfTransmittal f (ReportView x1
                                                                   x2
                                                                   x3
                                                                   x4
                                                                   x5
                                                                   x6
                                                                   x7
                                                                   x8
                                                                   x9
                                                                   x10
                                                                   x11
                                                                   x12
                                                                   x13
                                                                   x14
                                                                   x15
                                                                   x16
                                                                   x17
                                                                   x18
                                                                   x19
                                                                   x20
                                                                   x21
                                                                   x22
                                                                   x23
                                                                   x24
                                                                   x25
                                                                   x26
                                                                   x27
                                                                   x28
                                                                   x29
                                                                   x30
                                                                   x31
                                                                   x32
                                                                   x33
                                                                   x34
                                                                   x35
                                                                   x36
                                                                   x37
                                                                   x38
                                                                   x39
                                                                   x40
                                                                   x41
                                                                   x42
                                                                   x43
                                                                   x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 y1 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x30)
          {-# INLINE lens_ReportView__reportLetterOfTransmittal #-}
          lens_ReportView__reportLimitingConditions f (ReportView x1
                                                                  x2
                                                                  x3
                                                                  x4
                                                                  x5
                                                                  x6
                                                                  x7
                                                                  x8
                                                                  x9
                                                                  x10
                                                                  x11
                                                                  x12
                                                                  x13
                                                                  x14
                                                                  x15
                                                                  x16
                                                                  x17
                                                                  x18
                                                                  x19
                                                                  x20
                                                                  x21
                                                                  x22
                                                                  x23
                                                                  x24
                                                                  x25
                                                                  x26
                                                                  x27
                                                                  x28
                                                                  x29
                                                                  x30
                                                                  x31
                                                                  x32
                                                                  x33
                                                                  x34
                                                                  x35
                                                                  x36
                                                                  x37
                                                                  x38
                                                                  x39
                                                                  x40
                                                                  x41
                                                                  x42
                                                                  x43
                                                                  x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 y1 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x33)
          {-# INLINE lens_ReportView__reportLimitingConditions #-}
          lens_ReportView__reportName f (ReportView x1
                                                    x2
                                                    x3
                                                    x4
                                                    x5
                                                    x6
                                                    x7
                                                    x8
                                                    x9
                                                    x10
                                                    x11
                                                    x12
                                                    x13
                                                    x14
                                                    x15
                                                    x16
                                                    x17
                                                    x18
                                                    x19
                                                    x20
                                                    x21
                                                    x22
                                                    x23
                                                    x24
                                                    x25
                                                    x26
                                                    x27
                                                    x28
                                                    x29
                                                    x30
                                                    x31
                                                    x32
                                                    x33
                                                    x34
                                                    x35
                                                    x36
                                                    x37
                                                    x38
                                                    x39
                                                    x40
                                                    x41
                                                    x42
                                                    x43
                                                    x44) = fmap (\y1 -> ReportView x1 y1 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x2)
          {-# INLINE lens_ReportView__reportName #-}
          lens_ReportView__reportOrderByItemName f (ReportView x1
                                                               x2
                                                               x3
                                                               x4
                                                               x5
                                                               x6
                                                               x7
                                                               x8
                                                               x9
                                                               x10
                                                               x11
                                                               x12
                                                               x13
                                                               x14
                                                               x15
                                                               x16
                                                               x17
                                                               x18
                                                               x19
                                                               x20
                                                               x21
                                                               x22
                                                               x23
                                                               x24
                                                               x25
                                                               x26
                                                               x27
                                                               x28
                                                               x29
                                                               x30
                                                               x31
                                                               x32
                                                               x33
                                                               x34
                                                               x35
                                                               x36
                                                               x37
                                                               x38
                                                               x39
                                                               x40
                                                               x41
                                                               x42
                                                               x43
                                                               x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 y1 x44) (f x43)
          {-# INLINE lens_ReportView__reportOrderByItemName #-}
          lens_ReportView__reportPerms f (ReportView x1
                                                     x2
                                                     x3
                                                     x4
                                                     x5
                                                     x6
                                                     x7
                                                     x8
                                                     x9
                                                     x10
                                                     x11
                                                     x12
                                                     x13
                                                     x14
                                                     x15
                                                     x16
                                                     x17
                                                     x18
                                                     x19
                                                     x20
                                                     x21
                                                     x22
                                                     x23
                                                     x24
                                                     x25
                                                     x26
                                                     x27
                                                     x28
                                                     x29
                                                     x30
                                                     x31
                                                     x32
                                                     x33
                                                     x34
                                                     x35
                                                     x36
                                                     x37
                                                     x38
                                                     x39
                                                     x40
                                                     x41
                                                     x42
                                                     x43
                                                     x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 y1 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x35)
          {-# INLINE lens_ReportView__reportPerms #-}
          lens_ReportView__reportPreparer f (ReportView x1
                                                        x2
                                                        x3
                                                        x4
                                                        x5
                                                        x6
                                                        x7
                                                        x8
                                                        x9
                                                        x10
                                                        x11
                                                        x12
                                                        x13
                                                        x14
                                                        x15
                                                        x16
                                                        x17
                                                        x18
                                                        x19
                                                        x20
                                                        x21
                                                        x22
                                                        x23
                                                        x24
                                                        x25
                                                        x26
                                                        x27
                                                        x28
                                                        x29
                                                        x30
                                                        x31
                                                        x32
                                                        x33
                                                        x34
                                                        x35
                                                        x36
                                                        x37
                                                        x38
                                                        x39
                                                        x40
                                                        x41
                                                        x42
                                                        x43
                                                        x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 y1 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x8)
          {-# INLINE lens_ReportView__reportPreparer #-}
          lens_ReportView__reportPreparerAddress f (ReportView x1
                                                               x2
                                                               x3
                                                               x4
                                                               x5
                                                               x6
                                                               x7
                                                               x8
                                                               x9
                                                               x10
                                                               x11
                                                               x12
                                                               x13
                                                               x14
                                                               x15
                                                               x16
                                                               x17
                                                               x18
                                                               x19
                                                               x20
                                                               x21
                                                               x22
                                                               x23
                                                               x24
                                                               x25
                                                               x26
                                                               x27
                                                               x28
                                                               x29
                                                               x30
                                                               x31
                                                               x32
                                                               x33
                                                               x34
                                                               x35
                                                               x36
                                                               x37
                                                               x38
                                                               x39
                                                               x40
                                                               x41
                                                               x42
                                                               x43
                                                               x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 y1 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x10)
          {-# INLINE lens_ReportView__reportPreparerAddress #-}
          lens_ReportView__reportPreparerEIN f (ReportView x1
                                                           x2
                                                           x3
                                                           x4
                                                           x5
                                                           x6
                                                           x7
                                                           x8
                                                           x9
                                                           x10
                                                           x11
                                                           x12
                                                           x13
                                                           x14
                                                           x15
                                                           x16
                                                           x17
                                                           x18
                                                           x19
                                                           x20
                                                           x21
                                                           x22
                                                           x23
                                                           x24
                                                           x25
                                                           x26
                                                           x27
                                                           x28
                                                           x29
                                                           x30
                                                           x31
                                                           x32
                                                           x33
                                                           x34
                                                           x35
                                                           x36
                                                           x37
                                                           x38
                                                           x39
                                                           x40
                                                           x41
                                                           x42
                                                           x43
                                                           x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 y1 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x9)
          {-# INLINE lens_ReportView__reportPreparerEIN #-}
          lens_ReportView__reportPreparerEMail f (ReportView x1
                                                             x2
                                                             x3
                                                             x4
                                                             x5
                                                             x6
                                                             x7
                                                             x8
                                                             x9
                                                             x10
                                                             x11
                                                             x12
                                                             x13
                                                             x14
                                                             x15
                                                             x16
                                                             x17
                                                             x18
                                                             x19
                                                             x20
                                                             x21
                                                             x22
                                                             x23
                                                             x24
                                                             x25
                                                             x26
                                                             x27
                                                             x28
                                                             x29
                                                             x30
                                                             x31
                                                             x32
                                                             x33
                                                             x34
                                                             x35
                                                             x36
                                                             x37
                                                             x38
                                                             x39
                                                             x40
                                                             x41
                                                             x42
                                                             x43
                                                             x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 y1 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x11)
          {-# INLINE lens_ReportView__reportPreparerEMail #-}
          lens_ReportView__reportPreparerWebsite f (ReportView x1
                                                               x2
                                                               x3
                                                               x4
                                                               x5
                                                               x6
                                                               x7
                                                               x8
                                                               x9
                                                               x10
                                                               x11
                                                               x12
                                                               x13
                                                               x14
                                                               x15
                                                               x16
                                                               x17
                                                               x18
                                                               x19
                                                               x20
                                                               x21
                                                               x22
                                                               x23
                                                               x24
                                                               x25
                                                               x26
                                                               x27
                                                               x28
                                                               x29
                                                               x30
                                                               x31
                                                               x32
                                                               x33
                                                               x34
                                                               x35
                                                               x36
                                                               x37
                                                               x38
                                                               x39
                                                               x40
                                                               x41
                                                               x42
                                                               x43
                                                               x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 y1 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x12)
          {-# INLINE lens_ReportView__reportPreparerWebsite #-}
          lens_ReportView__reportPrivacyPolicy f (ReportView x1
                                                             x2
                                                             x3
                                                             x4
                                                             x5
                                                             x6
                                                             x7
                                                             x8
                                                             x9
                                                             x10
                                                             x11
                                                             x12
                                                             x13
                                                             x14
                                                             x15
                                                             x16
                                                             x17
                                                             x18
                                                             x19
                                                             x20
                                                             x21
                                                             x22
                                                             x23
                                                             x24
                                                             x25
                                                             x26
                                                             x27
                                                             x28
                                                             x29
                                                             x30
                                                             x31
                                                             x32
                                                             x33
                                                             x34
                                                             x35
                                                             x36
                                                             x37
                                                             x38
                                                             x39
                                                             x40
                                                             x41
                                                             x42
                                                             x43
                                                             x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 y1 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x34)
          {-# INLINE lens_ReportView__reportPrivacyPolicy #-}
          lens_ReportView__reportRedacted f (ReportView x1
                                                        x2
                                                        x3
                                                        x4
                                                        x5
                                                        x6
                                                        x7
                                                        x8
                                                        x9
                                                        x10
                                                        x11
                                                        x12
                                                        x13
                                                        x14
                                                        x15
                                                        x16
                                                        x17
                                                        x18
                                                        x19
                                                        x20
                                                        x21
                                                        x22
                                                        x23
                                                        x24
                                                        x25
                                                        x26
                                                        x27
                                                        x28
                                                        x29
                                                        x30
                                                        x31
                                                        x32
                                                        x33
                                                        x34
                                                        x35
                                                        x36
                                                        x37
                                                        x38
                                                        x39
                                                        x40
                                                        x41
                                                        x42
                                                        x43
                                                        x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 y1 x41 x42 x43 x44) (f x40)
          {-# INLINE lens_ReportView__reportRedacted #-}
          lens_ReportView__reportRevision f (ReportView x1
                                                        x2
                                                        x3
                                                        x4
                                                        x5
                                                        x6
                                                        x7
                                                        x8
                                                        x9
                                                        x10
                                                        x11
                                                        x12
                                                        x13
                                                        x14
                                                        x15
                                                        x16
                                                        x17
                                                        x18
                                                        x19
                                                        x20
                                                        x21
                                                        x22
                                                        x23
                                                        x24
                                                        x25
                                                        x26
                                                        x27
                                                        x28
                                                        x29
                                                        x30
                                                        x31
                                                        x32
                                                        x33
                                                        x34
                                                        x35
                                                        x36
                                                        x37
                                                        x38
                                                        x39
                                                        x40
                                                        x41
                                                        x42
                                                        x43
                                                        x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 y1 x37 x38 x39 x40 x41 x42 x43 x44) (f x36)
          {-# INLINE lens_ReportView__reportRevision #-}
          lens_ReportView__reportScopeOfWork f (ReportView x1
                                                           x2
                                                           x3
                                                           x4
                                                           x5
                                                           x6
                                                           x7
                                                           x8
                                                           x9
                                                           x10
                                                           x11
                                                           x12
                                                           x13
                                                           x14
                                                           x15
                                                           x16
                                                           x17
                                                           x18
                                                           x19
                                                           x20
                                                           x21
                                                           x22
                                                           x23
                                                           x24
                                                           x25
                                                           x26
                                                           x27
                                                           x28
                                                           x29
                                                           x30
                                                           x31
                                                           x32
                                                           x33
                                                           x34
                                                           x35
                                                           x36
                                                           x37
                                                           x38
                                                           x39
                                                           x40
                                                           x41
                                                           x42
                                                           x43
                                                           x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 y1 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x31)
          {-# INLINE lens_ReportView__reportScopeOfWork #-}
          lens_ReportView__reportSources f (ReportView x1
                                                       x2
                                                       x3
                                                       x4
                                                       x5
                                                       x6
                                                       x7
                                                       x8
                                                       x9
                                                       x10
                                                       x11
                                                       x12
                                                       x13
                                                       x14
                                                       x15
                                                       x16
                                                       x17
                                                       x18
                                                       x19
                                                       x20
                                                       x21
                                                       x22
                                                       x23
                                                       x24
                                                       x25
                                                       x26
                                                       x27
                                                       x28
                                                       x29
                                                       x30
                                                       x31
                                                       x32
                                                       x33
                                                       x34
                                                       x35
                                                       x36
                                                       x37
                                                       x38
                                                       x39
                                                       x40
                                                       x41
                                                       x42
                                                       x43
                                                       x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 y1 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x29)
          {-# INLINE lens_ReportView__reportSources #-}
          lens_ReportView__reportStatus f (ReportView x1
                                                      x2
                                                      x3
                                                      x4
                                                      x5
                                                      x6
                                                      x7
                                                      x8
                                                      x9
                                                      x10
                                                      x11
                                                      x12
                                                      x13
                                                      x14
                                                      x15
                                                      x16
                                                      x17
                                                      x18
                                                      x19
                                                      x20
                                                      x21
                                                      x22
                                                      x23
                                                      x24
                                                      x25
                                                      x26
                                                      x27
                                                      x28
                                                      x29
                                                      x30
                                                      x31
                                                      x32
                                                      x33
                                                      x34
                                                      x35
                                                      x36
                                                      x37
                                                      x38
                                                      x39
                                                      x40
                                                      x41
                                                      x42
                                                      x43
                                                      x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 y1 x40 x41 x42 x43 x44) (f x39)
          {-# INLINE lens_ReportView__reportStatus #-}
          lens_ReportView__reportTitle f (ReportView x1
                                                     x2
                                                     x3
                                                     x4
                                                     x5
                                                     x6
                                                     x7
                                                     x8
                                                     x9
                                                     x10
                                                     x11
                                                     x12
                                                     x13
                                                     x14
                                                     x15
                                                     x16
                                                     x17
                                                     x18
                                                     x19
                                                     x20
                                                     x21
                                                     x22
                                                     x23
                                                     x24
                                                     x25
                                                     x26
                                                     x27
                                                     x28
                                                     x29
                                                     x30
                                                     x31
                                                     x32
                                                     x33
                                                     x34
                                                     x35
                                                     x36
                                                     x37
                                                     x38
                                                     x39
                                                     x40
                                                     x41
                                                     x42
                                                     x43
                                                     x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 y1 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x14)
          {-# INLINE lens_ReportView__reportTitle #-}
          lens_ReportView__reportUUID f (ReportView x1
                                                    x2
                                                    x3
                                                    x4
                                                    x5
                                                    x6
                                                    x7
                                                    x8
                                                    x9
                                                    x10
                                                    x11
                                                    x12
                                                    x13
                                                    x14
                                                    x15
                                                    x16
                                                    x17
                                                    x18
                                                    x19
                                                    x20
                                                    x21
                                                    x22
                                                    x23
                                                    x24
                                                    x25
                                                    x26
                                                    x27
                                                    x28
                                                    x29
                                                    x30
                                                    x31
                                                    x32
                                                    x33
                                                    x34
                                                    x35
                                                    x36
                                                    x37
                                                    x38
                                                    x39
                                                    x40
                                                    x41
                                                    x42
                                                    x43
                                                    x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 y1 x43 x44) (f x42)
          {-# INLINE lens_ReportView__reportUUID #-}
          lens_ReportView__reportValueApproachInfo f (ReportView x1
                                                                 x2
                                                                 x3
                                                                 x4
                                                                 x5
                                                                 x6
                                                                 x7
                                                                 x8
                                                                 x9
                                                                 x10
                                                                 x11
                                                                 x12
                                                                 x13
                                                                 x14
                                                                 x15
                                                                 x16
                                                                 x17
                                                                 x18
                                                                 x19
                                                                 x20
                                                                 x21
                                                                 x22
                                                                 x23
                                                                 x24
                                                                 x25
                                                                 x26
                                                                 x27
                                                                 x28
                                                                 x29
                                                                 x30
                                                                 x31
                                                                 x32
                                                                 x33
                                                                 x34
                                                                 x35
                                                                 x36
                                                                 x37
                                                                 x38
                                                                 x39
                                                                 x40
                                                                 x41
                                                                 x42
                                                                 x43
                                                                 x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 y1 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x19)
          {-# INLINE lens_ReportView__reportValueApproachInfo #-}
          lens_ReportView__reportValueTypeInfo f (ReportView x1
                                                             x2
                                                             x3
                                                             x4
                                                             x5
                                                             x6
                                                             x7
                                                             x8
                                                             x9
                                                             x10
                                                             x11
                                                             x12
                                                             x13
                                                             x14
                                                             x15
                                                             x16
                                                             x17
                                                             x18
                                                             x19
                                                             x20
                                                             x21
                                                             x22
                                                             x23
                                                             x24
                                                             x25
                                                             x26
                                                             x27
                                                             x28
                                                             x29
                                                             x30
                                                             x31
                                                             x32
                                                             x33
                                                             x34
                                                             x35
                                                             x36
                                                             x37
                                                             x38
                                                             x39
                                                             x40
                                                             x41
                                                             x42
                                                             x43
                                                             x44) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 y1 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x18)
          {-# INLINE lens_ReportView__reportValueTypeInfo #-}
instance HasText Text
    where lens_text = id
instance HasUnits Units
    where lens_units = id
instance Path (Either URI ImageFile) (Either URI ImageFile)
    where type PathType (Either URI ImageFile)
                        (Either URI ImageFile) = Path_Either (Path_URI (Either URI
                                                                               ImageFile))
                                                             (Path_ImageFile (Either URI ImageFile))
          toLens _ = iso id id
instance Path (Either URI ImageFile) ImageFile
    where type PathType (Either URI ImageFile)
                        ImageFile = Path_Either (Path_URI ImageFile)
                                                (Path_ImageFile ImageFile)
          toLens (Path_Right _) = _Right
          toLens u = error $ ("Unexpected goal ImageFile for Either URI ImageFile: " ++ show u)
instance Path (Either URI ImageFile) URI
    where type PathType (Either URI ImageFile)
                        URI = Path_Either (Path_URI URI) (Path_ImageFile URI)
          toLens (Path_Left _) = _Left
          toLens u = error $ ("Unexpected goal URI for Either URI ImageFile: " ++ show u)
instance Path (Map ItemFieldName Markup) (Map ItemFieldName Markup)
    where type PathType (Map ItemFieldName Markup)
                        (Map ItemFieldName Markup) = Path_Map ItemFieldName
                                                              (Path_Markup (Map ItemFieldName
                                                                                Markup))
          toLens _ = iso id id
instance Path (Map ItemFieldName Markup) JSONText
    where type PathType (Map ItemFieldName Markup)
                        JSONText = Path_Map ItemFieldName (Path_Markup JSONText)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Map ItemFieldName Markup: " ++ show u)
instance Path (Map ItemFieldName Markup) Markup
    where type PathType (Map ItemFieldName Markup)
                        Markup = Path_Map ItemFieldName (Path_Markup Markup)
          toLens (Path_Look k _) = mat k
          toLens u = error $ ("Unexpected goal Markup for Map ItemFieldName Markup: " ++ show u)
instance Path (Map ItemFieldName Markup) Text
    where type PathType (Map ItemFieldName Markup)
                        Text = Path_Map ItemFieldName (Path_Markup Text)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Text for Map ItemFieldName Markup: " ++ show u)
instance Path (Map ReportID Report) (Either URI ImageFile)
    where type PathType (Map ReportID Report)
                        (Either URI ImageFile) = Path_Map ReportID
                                                          (Path_Report (Either URI ImageFile))
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Either URI ImageFile for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) (Map ItemFieldName Markup)
    where type PathType (Map ReportID Report)
                        (Map ItemFieldName Markup) = Path_Map ReportID
                                                              (Path_Report (Map ItemFieldName
                                                                                Markup))
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Map ItemFieldName Markup for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) (Map ReportID Report)
    where type PathType (Map ReportID Report)
                        (Map ReportID Report) = Path_Map ReportID
                                                         (Path_Report (Map ReportID Report))
          toLens _ = iso id id
instance Path (Map ReportID Report) (Maybe (Either URI ImageFile))
    where type PathType (Map ReportID Report)
                        (Maybe (Either URI ImageFile)) = Path_Map ReportID
                                                                  (Path_Report (Maybe (Either URI
                                                                                              ImageFile)))
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Maybe (Either URI ImageFile) for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) AbbrevPair
    where type PathType (Map ReportID Report)
                        AbbrevPair = Path_Map ReportID (Path_Report AbbrevPair)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal (CIString, Markup) (aka AbbrevPair) for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) AbbrevPairs
    where type PathType (Map ReportID Report)
                        AbbrevPairs = Path_Map ReportID (Path_Report AbbrevPairs)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Order AbbrevPairID ((CIString, Markup)) (aka AbbrevPairs) for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) Author
    where type PathType (Map ReportID Report)
                        Author = Path_Map ReportID (Path_Report Author)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Author for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) Authors
    where type PathType (Map ReportID Report)
                        Authors = Path_Map ReportID (Path_Report Authors)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Order AuthorID Author (aka Authors) for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) Bool
    where type PathType (Map ReportID Report) Bool = Path_Map ReportID
                                                              (Path_Report Bool)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Bool for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) Branding
    where type PathType (Map ReportID Report)
                        Branding = Path_Map ReportID (Path_Report Branding)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Branding for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) CIString
    where type PathType (Map ReportID Report)
                        CIString = Path_Map ReportID (Path_Report CIString)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal CIString for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) Dimension
    where type PathType (Map ReportID Report)
                        Dimension = Path_Map ReportID (Path_Report Dimension)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Dimension for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) Double
    where type PathType (Map ReportID Report)
                        Double = Path_Map ReportID (Path_Report Double)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Double for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) ImageCrop
    where type PathType (Map ReportID Report)
                        ImageCrop = Path_Map ReportID (Path_Report ImageCrop)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal ImageCrop for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) ImageFile
    where type PathType (Map ReportID Report)
                        ImageFile = Path_Map ReportID (Path_Report ImageFile)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal ImageFile for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) ImageSize
    where type PathType (Map ReportID Report)
                        ImageSize = Path_Map ReportID (Path_Report ImageSize)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal ImageSize for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) Int64
    where type PathType (Map ReportID Report) Int64 = Path_Map ReportID
                                                               (Path_Report Int64)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Int64 (aka EpochMilli) for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) Integer
    where type PathType (Map ReportID Report)
                        Integer = Path_Map ReportID (Path_Report Integer)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Integer for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) Item
    where type PathType (Map ReportID Report) Item = Path_Map ReportID
                                                              (Path_Report Item)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Item for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) JSONText
    where type PathType (Map ReportID Report)
                        JSONText = Path_Map ReportID (Path_Report JSONText)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) Markup
    where type PathType (Map ReportID Report)
                        Markup = Path_Map ReportID (Path_Report Markup)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Markup for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) MarkupPair
    where type PathType (Map ReportID Report)
                        MarkupPair = Path_Map ReportID (Path_Report MarkupPair)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal (Markup, Markup) (aka MarkupPair) for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) MarkupPairs
    where type PathType (Map ReportID Report)
                        MarkupPairs = Path_Map ReportID (Path_Report MarkupPairs)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Order MarkupPairID ((Markup, Markup)) (aka MarkupPairs) for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) Markups
    where type PathType (Map ReportID Report)
                        Markups = Path_Map ReportID (Path_Report Markups)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Order MarkupID Markup (aka Markups) for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) MaybeImageFile
    where type PathType (Map ReportID Report)
                        MaybeImageFile = Path_Map ReportID (Path_Report MaybeImageFile)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Maybe ImageFile (aka MaybeImageFile) for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) MaybeReportIntendedUse
    where type PathType (Map ReportID Report)
                        MaybeReportIntendedUse = Path_Map ReportID
                                                          (Path_Report MaybeReportIntendedUse)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Maybe ReportIntendedUse (aka MaybeReportIntendedUse) for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) Permissions
    where type PathType (Map ReportID Report)
                        Permissions = Path_Map ReportID (Path_Report Permissions)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Permissions for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) ReadOnlyFilePath
    where type PathType (Map ReportID Report)
                        ReadOnlyFilePath = Path_Map ReportID (Path_Report ReadOnlyFilePath)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal ReadOnly ([Char]) (aka ReadOnlyFilePath) for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) Report
    where type PathType (Map ReportID Report)
                        Report = Path_Map ReportID (Path_Report Report)
          toLens (Path_Look k _) = mat k
          toLens u = error $ ("Unexpected goal Report for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) ReportElem
    where type PathType (Map ReportID Report)
                        ReportElem = Path_Map ReportID (Path_Report ReportElem)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal ReportElem for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) ReportElems
    where type PathType (Map ReportID Report)
                        ReportElems = Path_Map ReportID (Path_Report ReportElems)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Order ReportElemID ReportElem (aka ReportElems) for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) ReportFlags
    where type PathType (Map ReportID Report)
                        ReportFlags = Path_Map ReportID (Path_Report ReportFlags)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal ReportFlags for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) ReportImage
    where type PathType (Map ReportID Report)
                        ReportImage = Path_Map ReportID (Path_Report ReportImage)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal ReportImage for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) ReportImageView
    where type PathType (Map ReportID Report)
                        ReportImageView = Path_Map ReportID (Path_Report ReportImageView)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal ReportImageView for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) ReportImages
    where type PathType (Map ReportID Report)
                        ReportImages = Path_Map ReportID (Path_Report ReportImages)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Order ReportImageID ReportImage (aka ReportImages) for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) ReportStatus
    where type PathType (Map ReportID Report)
                        ReportStatus = Path_Map ReportID (Path_Report ReportStatus)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal ReportStatus for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) ReportValueApproachInfo
    where type PathType (Map ReportID Report)
                        ReportValueApproachInfo = Path_Map ReportID
                                                           (Path_Report ReportValueApproachInfo)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal ReportValueApproachInfo for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) ReportValueTypeInfo
    where type PathType (Map ReportID Report)
                        ReportValueTypeInfo = Path_Map ReportID
                                                       (Path_Report ReportValueTypeInfo)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal ReportValueTypeInfo for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) ReportView
    where type PathType (Map ReportID Report)
                        ReportView = Path_Map ReportID (Path_Report ReportView)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal ReportView for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) SaneSizeImageSize
    where type PathType (Map ReportID Report)
                        SaneSizeImageSize = Path_Map ReportID
                                                     (Path_Report SaneSizeImageSize)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal SaneSize ImageSize (aka SaneSizeImageSize) for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) String
    where type PathType (Map ReportID Report)
                        String = Path_Map ReportID (Path_Report String)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal [Char] (aka String, aka FilePath, aka Checksum) for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) Text
    where type PathType (Map ReportID Report) Text = Path_Map ReportID
                                                              (Path_Report Text)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Text for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) URI
    where type PathType (Map ReportID Report) URI = Path_Map ReportID
                                                             (Path_Report URI)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal URI for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) UUID
    where type PathType (Map ReportID Report) UUID = Path_Map ReportID
                                                              (Path_Report UUID)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal UUID for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) Units
    where type PathType (Map ReportID Report) Units = Path_Map ReportID
                                                               (Path_Report Units)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Units for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) UserId
    where type PathType (Map ReportID Report)
                        UserId = Path_Map ReportID (Path_Report UserId)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal UserId for Map ReportID Report: " ++ show u)
instance Path (Map ReportID Report) UserIds
    where type PathType (Map ReportID Report)
                        UserIds = Path_Map ReportID (Path_Report UserIds)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal [UserId] (aka UserIds) for Map ReportID Report: " ++ show u)
instance Path (Maybe (Either URI ImageFile)) (Either URI ImageFile)
    where type PathType (Maybe (Either URI ImageFile))
                        (Either URI
                                ImageFile) = Path_Maybe (Path_Either (Path_URI (Either URI
                                                                                       ImageFile))
                                                                     (Path_ImageFile (Either URI
                                                                                             ImageFile)))
          toLens (Path_Just _) = _Just
          toLens u = error $ ("Unexpected goal Either URI ImageFile for Maybe (Either URI ImageFile): " ++ show u)
instance Path (Maybe (Either URI ImageFile))
              (Maybe (Either URI ImageFile))
    where type PathType (Maybe (Either URI ImageFile))
                        (Maybe (Either URI
                                       ImageFile)) = Path_Maybe (Path_Either (Path_URI (Maybe (Either URI
                                                                                                      ImageFile)))
                                                                             (Path_ImageFile (Maybe (Either URI
                                                                                                            ImageFile))))
          toLens _ = iso id id
instance Path (Maybe (Either URI ImageFile)) ImageFile
    where type PathType (Maybe (Either URI ImageFile))
                        ImageFile = Path_Maybe (Path_Either (Path_URI ImageFile)
                                                            (Path_ImageFile ImageFile))
          toLens (Path_Just v) = _Just . toLens v
          toLens u = error $ ("Unexpected goal ImageFile for Maybe (Either URI ImageFile): " ++ show u)
instance Path (Maybe (Either URI ImageFile)) URI
    where type PathType (Maybe (Either URI ImageFile))
                        URI = Path_Maybe (Path_Either (Path_URI URI) (Path_ImageFile URI))
          toLens (Path_Just v) = _Just . toLens v
          toLens u = error $ ("Unexpected goal URI for Maybe (Either URI ImageFile): " ++ show u)
instance Path AbbrevPair AbbrevPair
    where type PathType AbbrevPair
                        AbbrevPair = Path_Pair (Path_CIString AbbrevPair)
                                               (Path_Markup AbbrevPair)
          toLens _ = iso id id
instance Path AbbrevPair CIString
    where type PathType AbbrevPair
                        CIString = Path_Pair (Path_CIString CIString)
                                             (Path_Markup CIString)
          toLens (Path_First _) = _1
          toLens u = error $ ("Unexpected goal CIString for (CIString, Markup) (aka AbbrevPair): " ++ show u)
instance Path AbbrevPair JSONText
    where type PathType AbbrevPair
                        JSONText = Path_Pair (Path_CIString JSONText)
                                             (Path_Markup JSONText)
          toLens (Path_First v) = _1 . toLens v
          toLens (Path_Second v) = _2 . toLens v
          toLens u = error $ ("Unexpected goal JSONText for (CIString, Markup) (aka AbbrevPair): " ++ show u)
instance Path AbbrevPair Markup
    where type PathType AbbrevPair
                        Markup = Path_Pair (Path_CIString Markup) (Path_Markup Markup)
          toLens (Path_Second _) = _2
          toLens u = error $ ("Unexpected goal Markup for (CIString, Markup) (aka AbbrevPair): " ++ show u)
instance Path AbbrevPair Text
    where type PathType AbbrevPair
                        Text = Path_Pair (Path_CIString Text) (Path_Markup Text)
          toLens (Path_First v) = _1 . toLens v
          toLens (Path_Second v) = _2 . toLens v
          toLens u = error $ ("Unexpected goal Text for (CIString, Markup) (aka AbbrevPair): " ++ show u)
instance Path AbbrevPairs AbbrevPair
    where type PathType AbbrevPairs AbbrevPair = Path_OMap AbbrevPairID
                                                           (Path_Pair (Path_CIString AbbrevPair)
                                                                      (Path_Markup AbbrevPair))
          toLens (Path_At k _) = lens_omat k
          toLens u = error $ ("Unexpected goal (CIString, Markup) (aka AbbrevPair) for Order AbbrevPairID ((CIString, Markup)) (aka AbbrevPairs): " ++ show u)
instance Path AbbrevPairs AbbrevPairs
    where type PathType AbbrevPairs
                        AbbrevPairs = Path_OMap AbbrevPairID
                                                (Path_Pair (Path_CIString AbbrevPairs)
                                                           (Path_Markup AbbrevPairs))
          toLens _ = iso id id
instance Path AbbrevPairs CIString
    where type PathType AbbrevPairs CIString = Path_OMap AbbrevPairID
                                                         (Path_Pair (Path_CIString CIString)
                                                                    (Path_Markup CIString))
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal CIString for Order AbbrevPairID ((CIString, Markup)) (aka AbbrevPairs): " ++ show u)
instance Path AbbrevPairs JSONText
    where type PathType AbbrevPairs JSONText = Path_OMap AbbrevPairID
                                                         (Path_Pair (Path_CIString JSONText)
                                                                    (Path_Markup JSONText))
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Order AbbrevPairID ((CIString, Markup)) (aka AbbrevPairs): " ++ show u)
instance Path AbbrevPairs Markup
    where type PathType AbbrevPairs Markup = Path_OMap AbbrevPairID
                                                       (Path_Pair (Path_CIString Markup)
                                                                  (Path_Markup Markup))
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Markup for Order AbbrevPairID ((CIString, Markup)) (aka AbbrevPairs): " ++ show u)
instance Path AbbrevPairs Text
    where type PathType AbbrevPairs Text = Path_OMap AbbrevPairID
                                                     (Path_Pair (Path_CIString Text)
                                                                (Path_Markup Text))
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Text for Order AbbrevPairID ((CIString, Markup)) (aka AbbrevPairs): " ++ show u)
instance Path Author Author
    where type PathType Author Author = Path_Author Author
          toLens _ = iso id id
instance Path Author JSONText
    where type PathType Author JSONText = Path_Author JSONText
          toLens (Path_Author_authorName _x) = lens_Author_authorName . toLens _x
          toLens (Path_Author_authorCredentials _x) = lens_Author_authorCredentials . toLens _x
          toLens u = error $ ("Unexpected goal JSONText for Author: " ++ show u)
instance Path Author Markup
    where type PathType Author Markup = Path_Author Markup
          toLens (Path_Author_authorName _x) = lens_Author_authorName
          toLens (Path_Author_authorCredentials _x) = lens_Author_authorCredentials
          toLens u = error $ ("Unexpected goal Markup for Author: " ++ show u)
instance Path Author Text
    where type PathType Author Text = Path_Author Text
          toLens (Path_Author_authorName _x) = lens_Author_authorName . toLens _x
          toLens (Path_Author_authorCredentials _x) = lens_Author_authorCredentials . toLens _x
          toLens u = error $ ("Unexpected goal Text for Author: " ++ show u)
instance Path Authors Author
    where type PathType Authors Author = Path_OMap AuthorID
                                                   (Path_Author Author)
          toLens (Path_At k _) = lens_omat k
          toLens u = error $ ("Unexpected goal Author for Order AuthorID Author (aka Authors): " ++ show u)
instance Path Authors Authors
    where type PathType Authors Authors = Path_OMap AuthorID
                                                    (Path_Author Authors)
          toLens _ = iso id id
instance Path Authors JSONText
    where type PathType Authors JSONText = Path_OMap AuthorID
                                                     (Path_Author JSONText)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Order AuthorID Author (aka Authors): " ++ show u)
instance Path Authors Markup
    where type PathType Authors Markup = Path_OMap AuthorID
                                                   (Path_Author Markup)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Markup for Order AuthorID Author (aka Authors): " ++ show u)
instance Path Authors Text
    where type PathType Authors Text = Path_OMap AuthorID
                                                 (Path_Author Text)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Text for Order AuthorID Author (aka Authors): " ++ show u)
instance Path Bool Bool
    where type PathType Bool Bool = Path_Bool Bool
          toLens _ = iso id id
instance Path Bool JSONText
    where type PathType Bool JSONText = Path_Bool JSONText
          toLens (Path_Bool_View v) = (viewLens :: Lens' Bool
                                                         String) . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Bool: " ++ show u)
instance Path Bool String
    where type PathType Bool String = Path_Bool String
          toLens (Path_Bool_View _) = viewLens :: Lens' Bool String
          toLens u = error $ ("Unexpected goal [Char] (aka String, aka FilePath, aka Checksum) for Bool: " ++ show u)
instance Path Branding Branding
    where type PathType Branding Branding = Path_Branding Branding
          toLens _ = iso id id
instance Path Branding JSONText
    where type PathType Branding JSONText = Path_Branding JSONText
          toLens (Path_Branding_View v) = (viewLens :: Lens' Branding
                                                             Text) . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Branding: " ++ show u)
instance Path Branding Text
    where type PathType Branding Text = Path_Branding Text
          toLens (Path_Branding_View _) = viewLens :: Lens' Branding Text
          toLens u = error $ ("Unexpected goal Text for Branding: " ++ show u)
instance Path CIString CIString
    where type PathType CIString CIString = Path_CIString CIString
          toLens _ = iso id id
instance Path CIString JSONText
    where type PathType CIString JSONText = Path_CIString JSONText
          toLens (Path_CIString_View v) = (viewLens :: Lens' CIString
                                                             Text) . toLens v
          toLens u = error $ ("Unexpected goal JSONText for CIString: " ++ show u)
instance Path CIString Text
    where type PathType CIString Text = Path_CIString Text
          toLens (Path_CIString_View _) = viewLens :: Lens' CIString Text
          toLens u = error $ ("Unexpected goal Text for CIString: " ++ show u)
instance Path Dimension Dimension
    where type PathType Dimension Dimension = Path_Dimension Dimension
          toLens _ = iso id id
instance Path Dimension JSONText
    where type PathType Dimension JSONText = Path_Dimension JSONText
          toLens (Path_Dimension_View _) = viewLens :: Lens' Dimension
                                                             JSONText
          toLens u = error $ ("Unexpected goal JSONText for Dimension: " ++ show u)
instance Path Double Double
    where type PathType Double Double = Path_Double Double
          toLens _ = iso id id
instance Path Double JSONText
    where type PathType Double JSONText = Path_Double JSONText
          toLens (Path_Double_View v) = (viewLens :: Lens' Double
                                                           String) . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Double: " ++ show u)
instance Path Double String
    where type PathType Double String = Path_Double String
          toLens (Path_Double_View _) = viewLens :: Lens' Double String
          toLens u = error $ ("Unexpected goal [Char] (aka String, aka FilePath, aka Checksum) for Double: " ++ show u)
instance Path ImageCrop ImageCrop
    where type PathType ImageCrop ImageCrop = Path_ImageCrop ImageCrop
          toLens _ = iso id id
instance Path ImageFile ImageFile
    where type PathType ImageFile ImageFile = Path_ImageFile ImageFile
          toLens _ = iso id id
instance Path ImageSize Dimension
    where type PathType ImageSize Dimension = Path_ImageSize Dimension
          toLens (Path_ImageSize_dim _x) = lens_ImageSize_dim
          toLens u = error $ ("Unexpected goal Dimension for ImageSize: " ++ show u)
instance Path ImageSize Double
    where type PathType ImageSize Double = Path_ImageSize Double
          toLens (Path_ImageSize_size _x) = lens_ImageSize_size
          toLens u = error $ ("Unexpected goal Double for ImageSize: " ++ show u)
instance Path ImageSize ImageSize
    where type PathType ImageSize ImageSize = Path_ImageSize ImageSize
          toLens _ = iso id id
instance Path ImageSize JSONText
    where type PathType ImageSize JSONText = Path_ImageSize JSONText
          toLens (Path_ImageSize_dim _x) = lens_ImageSize_dim . toLens _x
          toLens (Path_ImageSize_size _x) = lens_ImageSize_size . toLens _x
          toLens (Path_ImageSize_units _x) = lens_ImageSize_units . toLens _x
          toLens u = error $ ("Unexpected goal JSONText for ImageSize: " ++ show u)
instance Path ImageSize String
    where type PathType ImageSize String = Path_ImageSize String
          toLens (Path_ImageSize_size _x) = lens_ImageSize_size . toLens _x
          toLens u = error $ ("Unexpected goal [Char] (aka String, aka FilePath, aka Checksum) for ImageSize: " ++ show u)
instance Path ImageSize Units
    where type PathType ImageSize Units = Path_ImageSize Units
          toLens (Path_ImageSize_units _x) = lens_ImageSize_units
          toLens u = error $ ("Unexpected goal Units for ImageSize: " ++ show u)
instance Path Int64 Int64
    where type PathType Int64 Int64 = Path_Int64 Int64
          toLens _ = iso id id
instance Path Integer Integer
    where type PathType Integer Integer = Path_Integer Integer
          toLens _ = iso id id
instance Path Item (Either URI ImageFile)
    where type PathType Item
                        (Either URI ImageFile) = Path_Item (Either URI ImageFile)
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal Either URI ImageFile for Item: " ++ show u)
instance Path Item (Map ItemFieldName Markup)
    where type PathType Item
                        (Map ItemFieldName Markup) = Path_Item (Map ItemFieldName Markup)
          toLens (Path_Item_fields _x) = lens_Item_fields
          toLens u = error $ ("Unexpected goal Map ItemFieldName Markup for Item: " ++ show u)
instance Path Item (Maybe (Either URI ImageFile))
    where type PathType Item
                        (Maybe (Either URI ImageFile)) = Path_Item (Maybe (Either URI
                                                                                  ImageFile))
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal Maybe (Either URI ImageFile) for Item: " ++ show u)
instance Path Item Bool
    where type PathType Item Bool = Path_Item Bool
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal Bool for Item: " ++ show u)
instance Path Item Dimension
    where type PathType Item Dimension = Path_Item Dimension
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal Dimension for Item: " ++ show u)
instance Path Item Double
    where type PathType Item Double = Path_Item Double
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal Double for Item: " ++ show u)
instance Path Item ImageCrop
    where type PathType Item ImageCrop = Path_Item ImageCrop
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal ImageCrop for Item: " ++ show u)
instance Path Item ImageFile
    where type PathType Item ImageFile = Path_Item ImageFile
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal ImageFile for Item: " ++ show u)
instance Path Item ImageSize
    where type PathType Item ImageSize = Path_Item ImageSize
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal ImageSize for Item: " ++ show u)
instance Path Item Item
    where type PathType Item Item = Path_Item Item
          toLens _ = iso id id
instance Path Item JSONText
    where type PathType Item JSONText = Path_Item JSONText
          toLens (Path_Item_itemName _x) = lens_Item_itemName . toLens _x
          toLens (Path_Item_fields _x) = lens_Item_fields . toLens _x
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal JSONText for Item: " ++ show u)
instance Path Item Markup
    where type PathType Item Markup = Path_Item Markup
          toLens (Path_Item_fields _x) = lens_Item_fields . toLens _x
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal Markup for Item: " ++ show u)
instance Path Item MaybeImageFile
    where type PathType Item MaybeImageFile = Path_Item MaybeImageFile
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal Maybe ImageFile (aka MaybeImageFile) for Item: " ++ show u)
instance Path Item ReportImage
    where type PathType Item ReportImage = Path_Item ReportImage
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal ReportImage for Item: " ++ show u)
instance Path Item ReportImageView
    where type PathType Item
                        ReportImageView = Path_Item ReportImageView
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal ReportImageView for Item: " ++ show u)
instance Path Item ReportImages
    where type PathType Item ReportImages = Path_Item ReportImages
          toLens (Path_Item_images _x) = lens_Item_images
          toLens u = error $ ("Unexpected goal Order ReportImageID ReportImage (aka ReportImages) for Item: " ++ show u)
instance Path Item SaneSizeImageSize
    where type PathType Item
                        SaneSizeImageSize = Path_Item SaneSizeImageSize
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal SaneSize ImageSize (aka SaneSizeImageSize) for Item: " ++ show u)
instance Path Item String
    where type PathType Item String = Path_Item String
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal [Char] (aka String, aka FilePath, aka Checksum) for Item: " ++ show u)
instance Path Item Text
    where type PathType Item Text = Path_Item Text
          toLens (Path_Item_itemName _x) = lens_Item_itemName
          toLens (Path_Item_fields _x) = lens_Item_fields . toLens _x
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal Text for Item: " ++ show u)
instance Path Item URI
    where type PathType Item URI = Path_Item URI
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal URI for Item: " ++ show u)
instance Path Item Units
    where type PathType Item Units = Path_Item Units
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal Units for Item: " ++ show u)
instance Path JSONText JSONText
    where type PathType JSONText JSONText = Path_JSONText JSONText
          toLens _ = iso id id
instance Path Markup JSONText
    where type PathType Markup JSONText = Path_Markup JSONText
          toLens (Path_Markup_markdownText _x) = lens_Markup_markdownText . toLens _x
          toLens (Path_Markup_htmlText _x) = lens_Markup_htmlText . toLens _x
          toLens u = error $ ("Unexpected goal JSONText for Markup: " ++ show u)
instance Path Markup Markup
    where type PathType Markup Markup = Path_Markup Markup
          toLens _ = iso id id
instance Path Markup Text
    where type PathType Markup Text = Path_Markup Text
          toLens (Path_Markup_markdownText _x) = lens_Markup_markdownText
          toLens (Path_Markup_htmlText _x) = lens_Markup_htmlText
          toLens u = error $ ("Unexpected goal Text for Markup: " ++ show u)
instance Path MarkupPair JSONText
    where type PathType MarkupPair
                        JSONText = Path_Pair (Path_Markup JSONText) (Path_Markup JSONText)
          toLens (Path_First v) = _1 . toLens v
          toLens (Path_Second v) = _2 . toLens v
          toLens u = error $ ("Unexpected goal JSONText for (Markup, Markup) (aka MarkupPair): " ++ show u)
instance Path MarkupPair Markup
    where type PathType MarkupPair
                        Markup = Path_Pair (Path_Markup Markup) (Path_Markup Markup)
          toLens (Path_First _) = _1
          toLens (Path_Second _) = _2
          toLens u = error $ ("Unexpected goal Markup for (Markup, Markup) (aka MarkupPair): " ++ show u)
instance Path MarkupPair MarkupPair
    where type PathType MarkupPair
                        MarkupPair = Path_Pair (Path_Markup MarkupPair)
                                               (Path_Markup MarkupPair)
          toLens _ = iso id id
instance Path MarkupPair Text
    where type PathType MarkupPair Text = Path_Pair (Path_Markup Text)
                                                    (Path_Markup Text)
          toLens (Path_First v) = _1 . toLens v
          toLens (Path_Second v) = _2 . toLens v
          toLens u = error $ ("Unexpected goal Text for (Markup, Markup) (aka MarkupPair): " ++ show u)
instance Path MarkupPairs JSONText
    where type PathType MarkupPairs JSONText = Path_OMap MarkupPairID
                                                         (Path_Pair (Path_Markup JSONText)
                                                                    (Path_Markup JSONText))
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Order MarkupPairID ((Markup, Markup)) (aka MarkupPairs): " ++ show u)
instance Path MarkupPairs Markup
    where type PathType MarkupPairs Markup = Path_OMap MarkupPairID
                                                       (Path_Pair (Path_Markup Markup)
                                                                  (Path_Markup Markup))
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Markup for Order MarkupPairID ((Markup, Markup)) (aka MarkupPairs): " ++ show u)
instance Path MarkupPairs MarkupPair
    where type PathType MarkupPairs MarkupPair = Path_OMap MarkupPairID
                                                           (Path_Pair (Path_Markup MarkupPair)
                                                                      (Path_Markup MarkupPair))
          toLens (Path_At k _) = lens_omat k
          toLens u = error $ ("Unexpected goal (Markup, Markup) (aka MarkupPair) for Order MarkupPairID ((Markup, Markup)) (aka MarkupPairs): " ++ show u)
instance Path MarkupPairs MarkupPairs
    where type PathType MarkupPairs
                        MarkupPairs = Path_OMap MarkupPairID
                                                (Path_Pair (Path_Markup MarkupPairs)
                                                           (Path_Markup MarkupPairs))
          toLens _ = iso id id
instance Path MarkupPairs Text
    where type PathType MarkupPairs Text = Path_OMap MarkupPairID
                                                     (Path_Pair (Path_Markup Text)
                                                                (Path_Markup Text))
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Text for Order MarkupPairID ((Markup, Markup)) (aka MarkupPairs): " ++ show u)
instance Path Markups JSONText
    where type PathType Markups JSONText = Path_OMap MarkupID
                                                     (Path_Markup JSONText)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Order MarkupID Markup (aka Markups): " ++ show u)
instance Path Markups Markup
    where type PathType Markups Markup = Path_OMap MarkupID
                                                   (Path_Markup Markup)
          toLens (Path_At k _) = lens_omat k
          toLens u = error $ ("Unexpected goal Markup for Order MarkupID Markup (aka Markups): " ++ show u)
instance Path Markups Markups
    where type PathType Markups Markups = Path_OMap MarkupID
                                                    (Path_Markup Markups)
          toLens _ = iso id id
instance Path Markups Text
    where type PathType Markups Text = Path_OMap MarkupID
                                                 (Path_Markup Text)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Text for Order MarkupID Markup (aka Markups): " ++ show u)
instance Path MaybeImageFile JSONText
    where type PathType MaybeImageFile
                        JSONText = Path_MaybeImageFile JSONText
          toLens (Path_MaybeImageFile_View v) = (viewLens :: Lens' (Maybe ImageFile)
                                                                   String) . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Maybe ImageFile (aka MaybeImageFile): " ++ show u)
instance Path MaybeImageFile MaybeImageFile
    where type PathType MaybeImageFile
                        MaybeImageFile = Path_MaybeImageFile MaybeImageFile
          toLens _ = iso id id
instance Path MaybeImageFile String
    where type PathType MaybeImageFile
                        String = Path_MaybeImageFile String
          toLens (Path_MaybeImageFile_View _) = viewLens :: Lens' (Maybe ImageFile)
                                                                  String
          toLens u = error $ ("Unexpected goal [Char] (aka String, aka FilePath, aka Checksum) for Maybe ImageFile (aka MaybeImageFile): " ++ show u)
instance Path MaybeReportIntendedUse JSONText
    where type PathType MaybeReportIntendedUse
                        JSONText = Path_MaybeReportIntendedUse JSONText
          toLens (Path_MaybeReportIntendedUse_View v) = (viewLens :: Lens' (Maybe ReportIntendedUse)
                                                                           String) . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Maybe ReportIntendedUse (aka MaybeReportIntendedUse): " ++ show u)
instance Path MaybeReportIntendedUse MaybeReportIntendedUse
    where type PathType MaybeReportIntendedUse
                        MaybeReportIntendedUse = Path_MaybeReportIntendedUse MaybeReportIntendedUse
          toLens _ = iso id id
instance Path MaybeReportIntendedUse String
    where type PathType MaybeReportIntendedUse
                        String = Path_MaybeReportIntendedUse String
          toLens (Path_MaybeReportIntendedUse_View _) = viewLens :: Lens' (Maybe ReportIntendedUse)
                                                                          String
          toLens u = error $ ("Unexpected goal [Char] (aka String, aka FilePath, aka Checksum) for Maybe ReportIntendedUse (aka MaybeReportIntendedUse): " ++ show u)
instance Path Permissions JSONText
    where type PathType Permissions
                        JSONText = Path_Permissions JSONText
          toLens (Path_Permissions_writers _x) = lens_Permissions_writers . toLens _x
          toLens (Path_Permissions_readers _x) = lens_Permissions_readers . toLens _x
          toLens u = error $ ("Unexpected goal JSONText for Permissions: " ++ show u)
instance Path Permissions Permissions
    where type PathType Permissions
                        Permissions = Path_Permissions Permissions
          toLens _ = iso id id
instance Path Permissions Text
    where type PathType Permissions Text = Path_Permissions Text
          toLens (Path_Permissions_writers _x) = lens_Permissions_writers . toLens _x
          toLens (Path_Permissions_readers _x) = lens_Permissions_readers . toLens _x
          toLens u = error $ ("Unexpected goal Text for Permissions: " ++ show u)
instance Path Permissions UserId
    where type PathType Permissions UserId = Path_Permissions UserId
          toLens (Path_Permissions_owner _x) = lens_Permissions_owner
          toLens u = error $ ("Unexpected goal UserId for Permissions: " ++ show u)
instance Path Permissions UserIds
    where type PathType Permissions UserIds = Path_Permissions UserIds
          toLens (Path_Permissions_writers _x) = lens_Permissions_writers
          toLens (Path_Permissions_readers _x) = lens_Permissions_readers
          toLens u = error $ ("Unexpected goal [UserId] (aka UserIds) for Permissions: " ++ show u)
instance Path ReadOnlyFilePath ReadOnlyFilePath
    where type PathType ReadOnlyFilePath
                        ReadOnlyFilePath = Path_ReadOnlyFilePath ReadOnlyFilePath
          toLens _ = iso id id
instance Path Report (Either URI ImageFile)
    where type PathType Report
                        (Either URI ImageFile) = Path_Report (Either URI ImageFile)
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Either URI ImageFile for Report: " ++ show u)
instance Path Report (Map ItemFieldName Markup)
    where type PathType Report
                        (Map ItemFieldName Markup) = Path_Report (Map ItemFieldName Markup)
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Map ItemFieldName Markup for Report: " ++ show u)
instance Path Report (Maybe (Either URI ImageFile))
    where type PathType Report
                        (Maybe (Either URI ImageFile)) = Path_Report (Maybe (Either URI
                                                                                    ImageFile))
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Maybe (Either URI ImageFile) for Report: " ++ show u)
instance Path Report AbbrevPair
    where type PathType Report AbbrevPair = Path_Report AbbrevPair
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal (CIString, Markup) (aka AbbrevPair) for Report: " ++ show u)
instance Path Report AbbrevPairs
    where type PathType Report AbbrevPairs = Path_Report AbbrevPairs
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Order AbbrevPairID ((CIString, Markup)) (aka AbbrevPairs) for Report: " ++ show u)
instance Path Report Author
    where type PathType Report Author = Path_Report Author
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Author for Report: " ++ show u)
instance Path Report Authors
    where type PathType Report Authors = Path_Report Authors
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Order AuthorID Author (aka Authors) for Report: " ++ show u)
instance Path Report Bool
    where type PathType Report Bool = Path_Report Bool
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Bool for Report: " ++ show u)
instance Path Report Branding
    where type PathType Report Branding = Path_Report Branding
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Branding for Report: " ++ show u)
instance Path Report CIString
    where type PathType Report CIString = Path_Report CIString
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal CIString for Report: " ++ show u)
instance Path Report Dimension
    where type PathType Report Dimension = Path_Report Dimension
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Dimension for Report: " ++ show u)
instance Path Report Double
    where type PathType Report Double = Path_Report Double
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Double for Report: " ++ show u)
instance Path Report ImageCrop
    where type PathType Report ImageCrop = Path_Report ImageCrop
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal ImageCrop for Report: " ++ show u)
instance Path Report ImageFile
    where type PathType Report ImageFile = Path_Report ImageFile
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal ImageFile for Report: " ++ show u)
instance Path Report ImageSize
    where type PathType Report ImageSize = Path_Report ImageSize
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal ImageSize for Report: " ++ show u)
instance Path Report Int64
    where type PathType Report Int64 = Path_Report Int64
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Int64 (aka EpochMilli) for Report: " ++ show u)
instance Path Report Integer
    where type PathType Report Integer = Path_Report Integer
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Integer for Report: " ++ show u)
instance Path Report Item
    where type PathType Report Item = Path_Report Item
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Item for Report: " ++ show u)
instance Path Report JSONText
    where type PathType Report JSONText = Path_Report JSONText
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Report: " ++ show u)
instance Path Report Markup
    where type PathType Report Markup = Path_Report Markup
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Markup for Report: " ++ show u)
instance Path Report MarkupPair
    where type PathType Report MarkupPair = Path_Report MarkupPair
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal (Markup, Markup) (aka MarkupPair) for Report: " ++ show u)
instance Path Report MarkupPairs
    where type PathType Report MarkupPairs = Path_Report MarkupPairs
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Order MarkupPairID ((Markup, Markup)) (aka MarkupPairs) for Report: " ++ show u)
instance Path Report Markups
    where type PathType Report Markups = Path_Report Markups
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Order MarkupID Markup (aka Markups) for Report: " ++ show u)
instance Path Report MaybeImageFile
    where type PathType Report
                        MaybeImageFile = Path_Report MaybeImageFile
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Maybe ImageFile (aka MaybeImageFile) for Report: " ++ show u)
instance Path Report MaybeReportIntendedUse
    where type PathType Report
                        MaybeReportIntendedUse = Path_Report MaybeReportIntendedUse
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Maybe ReportIntendedUse (aka MaybeReportIntendedUse) for Report: " ++ show u)
instance Path Report Permissions
    where type PathType Report Permissions = Path_Report Permissions
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Permissions for Report: " ++ show u)
instance Path Report ReadOnlyFilePath
    where type PathType Report
                        ReadOnlyFilePath = Path_Report ReadOnlyFilePath
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal ReadOnly ([Char]) (aka ReadOnlyFilePath) for Report: " ++ show u)
instance Path Report Report
    where type PathType Report Report = Path_Report Report
          toLens _ = iso id id
instance Path Report ReportElem
    where type PathType Report ReportElem = Path_Report ReportElem
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal ReportElem for Report: " ++ show u)
instance Path Report ReportElems
    where type PathType Report ReportElems = Path_Report ReportElems
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Order ReportElemID ReportElem (aka ReportElems) for Report: " ++ show u)
instance Path Report ReportFlags
    where type PathType Report ReportFlags = Path_Report ReportFlags
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal ReportFlags for Report: " ++ show u)
instance Path Report ReportImage
    where type PathType Report ReportImage = Path_Report ReportImage
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal ReportImage for Report: " ++ show u)
instance Path Report ReportImageView
    where type PathType Report
                        ReportImageView = Path_Report ReportImageView
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal ReportImageView for Report: " ++ show u)
instance Path Report ReportImages
    where type PathType Report ReportImages = Path_Report ReportImages
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Order ReportImageID ReportImage (aka ReportImages) for Report: " ++ show u)
instance Path Report ReportStatus
    where type PathType Report ReportStatus = Path_Report ReportStatus
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal ReportStatus for Report: " ++ show u)
instance Path Report ReportValueApproachInfo
    where type PathType Report
                        ReportValueApproachInfo = Path_Report ReportValueApproachInfo
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal ReportValueApproachInfo for Report: " ++ show u)
instance Path Report ReportValueTypeInfo
    where type PathType Report
                        ReportValueTypeInfo = Path_Report ReportValueTypeInfo
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal ReportValueTypeInfo for Report: " ++ show u)
instance Path Report ReportView
    where type PathType Report ReportView = Path_Report ReportView
          toLens (Path_Report_View _) = viewLens :: Lens' Report ReportView
          toLens u = error $ ("Unexpected goal ReportView for Report: " ++ show u)
instance Path Report SaneSizeImageSize
    where type PathType Report
                        SaneSizeImageSize = Path_Report SaneSizeImageSize
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal SaneSize ImageSize (aka SaneSizeImageSize) for Report: " ++ show u)
instance Path Report String
    where type PathType Report String = Path_Report String
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal [Char] (aka String, aka FilePath, aka Checksum) for Report: " ++ show u)
instance Path Report Text
    where type PathType Report Text = Path_Report Text
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Text for Report: " ++ show u)
instance Path Report URI
    where type PathType Report URI = Path_Report URI
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal URI for Report: " ++ show u)
instance Path Report UUID
    where type PathType Report UUID = Path_Report UUID
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal UUID for Report: " ++ show u)
instance Path Report Units
    where type PathType Report Units = Path_Report Units
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Units for Report: " ++ show u)
instance Path Report UserId
    where type PathType Report UserId = Path_Report UserId
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal UserId for Report: " ++ show u)
instance Path Report UserIds
    where type PathType Report UserIds = Path_Report UserIds
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal [UserId] (aka UserIds) for Report: " ++ show u)
instance Path ReportElem (Either URI ImageFile)
    where type PathType ReportElem
                        (Either URI ImageFile) = Path_ReportElem (Either URI ImageFile)
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal Either URI ImageFile for ReportElem: " ++ show u)
instance Path ReportElem (Map ItemFieldName Markup)
    where type PathType ReportElem
                        (Map ItemFieldName Markup) = Path_ReportElem (Map ItemFieldName
                                                                          Markup)
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal Map ItemFieldName Markup for ReportElem: " ++ show u)
instance Path ReportElem (Maybe (Either URI ImageFile))
    where type PathType ReportElem
                        (Maybe (Either URI ImageFile)) = Path_ReportElem (Maybe (Either URI
                                                                                        ImageFile))
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal Maybe (Either URI ImageFile) for ReportElem: " ++ show u)
instance Path ReportElem Bool
    where type PathType ReportElem Bool = Path_ReportElem Bool
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal Bool for ReportElem: " ++ show u)
instance Path ReportElem Dimension
    where type PathType ReportElem
                        Dimension = Path_ReportElem Dimension
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal Dimension for ReportElem: " ++ show u)
instance Path ReportElem Double
    where type PathType ReportElem Double = Path_ReportElem Double
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal Double for ReportElem: " ++ show u)
instance Path ReportElem ImageCrop
    where type PathType ReportElem
                        ImageCrop = Path_ReportElem ImageCrop
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal ImageCrop for ReportElem: " ++ show u)
instance Path ReportElem ImageFile
    where type PathType ReportElem
                        ImageFile = Path_ReportElem ImageFile
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal ImageFile for ReportElem: " ++ show u)
instance Path ReportElem ImageSize
    where type PathType ReportElem
                        ImageSize = Path_ReportElem ImageSize
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal ImageSize for ReportElem: " ++ show u)
instance Path ReportElem Item
    where type PathType ReportElem Item = Path_ReportElem Item
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem
          toLens u = error $ ("Unexpected goal Item for ReportElem: " ++ show u)
instance Path ReportElem JSONText
    where type PathType ReportElem JSONText = Path_ReportElem JSONText
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens (Path_ReportElem_elemText _x) = lens_ReportElem_elemText . toLens _x
          toLens u = error $ ("Unexpected goal JSONText for ReportElem: " ++ show u)
instance Path ReportElem Markup
    where type PathType ReportElem Markup = Path_ReportElem Markup
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens (Path_ReportElem_elemText _x) = lens_ReportElem_elemText
          toLens u = error $ ("Unexpected goal Markup for ReportElem: " ++ show u)
instance Path ReportElem MaybeImageFile
    where type PathType ReportElem
                        MaybeImageFile = Path_ReportElem MaybeImageFile
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal Maybe ImageFile (aka MaybeImageFile) for ReportElem: " ++ show u)
instance Path ReportElem ReportElem
    where type PathType ReportElem
                        ReportElem = Path_ReportElem ReportElem
          toLens _ = iso id id
instance Path ReportElem ReportImage
    where type PathType ReportElem
                        ReportImage = Path_ReportElem ReportImage
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal ReportImage for ReportElem: " ++ show u)
instance Path ReportElem ReportImageView
    where type PathType ReportElem
                        ReportImageView = Path_ReportElem ReportImageView
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal ReportImageView for ReportElem: " ++ show u)
instance Path ReportElem ReportImages
    where type PathType ReportElem
                        ReportImages = Path_ReportElem ReportImages
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal Order ReportImageID ReportImage (aka ReportImages) for ReportElem: " ++ show u)
instance Path ReportElem SaneSizeImageSize
    where type PathType ReportElem
                        SaneSizeImageSize = Path_ReportElem SaneSizeImageSize
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal SaneSize ImageSize (aka SaneSizeImageSize) for ReportElem: " ++ show u)
instance Path ReportElem String
    where type PathType ReportElem String = Path_ReportElem String
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal [Char] (aka String, aka FilePath, aka Checksum) for ReportElem: " ++ show u)
instance Path ReportElem Text
    where type PathType ReportElem Text = Path_ReportElem Text
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens (Path_ReportElem_elemText _x) = lens_ReportElem_elemText . toLens _x
          toLens u = error $ ("Unexpected goal Text for ReportElem: " ++ show u)
instance Path ReportElem URI
    where type PathType ReportElem URI = Path_ReportElem URI
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal URI for ReportElem: " ++ show u)
instance Path ReportElem Units
    where type PathType ReportElem Units = Path_ReportElem Units
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal Units for ReportElem: " ++ show u)
instance Path ReportElems (Either URI ImageFile)
    where type PathType ReportElems
                        (Either URI ImageFile) = Path_OMap ReportElemID
                                                           (Path_ReportElem (Either URI ImageFile))
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Either URI ImageFile for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance Path ReportElems (Map ItemFieldName Markup)
    where type PathType ReportElems
                        (Map ItemFieldName Markup) = Path_OMap ReportElemID
                                                               (Path_ReportElem (Map ItemFieldName
                                                                                     Markup))
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Map ItemFieldName Markup for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance Path ReportElems (Maybe (Either URI ImageFile))
    where type PathType ReportElems
                        (Maybe (Either URI ImageFile)) = Path_OMap ReportElemID
                                                                   (Path_ReportElem (Maybe (Either URI
                                                                                                   ImageFile)))
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Maybe (Either URI ImageFile) for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance Path ReportElems Bool
    where type PathType ReportElems Bool = Path_OMap ReportElemID
                                                     (Path_ReportElem Bool)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Bool for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance Path ReportElems Dimension
    where type PathType ReportElems Dimension = Path_OMap ReportElemID
                                                          (Path_ReportElem Dimension)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Dimension for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance Path ReportElems Double
    where type PathType ReportElems Double = Path_OMap ReportElemID
                                                       (Path_ReportElem Double)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Double for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance Path ReportElems ImageCrop
    where type PathType ReportElems ImageCrop = Path_OMap ReportElemID
                                                          (Path_ReportElem ImageCrop)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal ImageCrop for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance Path ReportElems ImageFile
    where type PathType ReportElems ImageFile = Path_OMap ReportElemID
                                                          (Path_ReportElem ImageFile)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal ImageFile for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance Path ReportElems ImageSize
    where type PathType ReportElems ImageSize = Path_OMap ReportElemID
                                                          (Path_ReportElem ImageSize)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal ImageSize for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance Path ReportElems Item
    where type PathType ReportElems Item = Path_OMap ReportElemID
                                                     (Path_ReportElem Item)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Item for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance Path ReportElems JSONText
    where type PathType ReportElems JSONText = Path_OMap ReportElemID
                                                         (Path_ReportElem JSONText)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance Path ReportElems Markup
    where type PathType ReportElems Markup = Path_OMap ReportElemID
                                                       (Path_ReportElem Markup)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Markup for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance Path ReportElems MaybeImageFile
    where type PathType ReportElems
                        MaybeImageFile = Path_OMap ReportElemID
                                                   (Path_ReportElem MaybeImageFile)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Maybe ImageFile (aka MaybeImageFile) for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance Path ReportElems ReportElem
    where type PathType ReportElems ReportElem = Path_OMap ReportElemID
                                                           (Path_ReportElem ReportElem)
          toLens (Path_At k _) = lens_omat k
          toLens u = error $ ("Unexpected goal ReportElem for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance Path ReportElems ReportElems
    where type PathType ReportElems
                        ReportElems = Path_OMap ReportElemID (Path_ReportElem ReportElems)
          toLens _ = iso id id
instance Path ReportElems ReportImage
    where type PathType ReportElems
                        ReportImage = Path_OMap ReportElemID (Path_ReportElem ReportImage)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal ReportImage for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance Path ReportElems ReportImageView
    where type PathType ReportElems
                        ReportImageView = Path_OMap ReportElemID
                                                    (Path_ReportElem ReportImageView)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal ReportImageView for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance Path ReportElems ReportImages
    where type PathType ReportElems
                        ReportImages = Path_OMap ReportElemID
                                                 (Path_ReportElem ReportImages)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Order ReportImageID ReportImage (aka ReportImages) for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance Path ReportElems SaneSizeImageSize
    where type PathType ReportElems
                        SaneSizeImageSize = Path_OMap ReportElemID
                                                      (Path_ReportElem SaneSizeImageSize)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal SaneSize ImageSize (aka SaneSizeImageSize) for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance Path ReportElems String
    where type PathType ReportElems String = Path_OMap ReportElemID
                                                       (Path_ReportElem String)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal [Char] (aka String, aka FilePath, aka Checksum) for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance Path ReportElems Text
    where type PathType ReportElems Text = Path_OMap ReportElemID
                                                     (Path_ReportElem Text)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Text for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance Path ReportElems URI
    where type PathType ReportElems URI = Path_OMap ReportElemID
                                                    (Path_ReportElem URI)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal URI for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance Path ReportElems Units
    where type PathType ReportElems Units = Path_OMap ReportElemID
                                                      (Path_ReportElem Units)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Units for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance Path ReportFlags Bool
    where type PathType ReportFlags Bool = Path_ReportFlags Bool
          toLens (Path_ReportFlags_hideEmptyItemFields _x) = lens_ReportFlags_hideEmptyItemFields
          toLens u = error $ ("Unexpected goal Bool for ReportFlags: " ++ show u)
instance Path ReportFlags JSONText
    where type PathType ReportFlags
                        JSONText = Path_ReportFlags JSONText
          toLens (Path_ReportFlags_hideEmptyItemFields _x) = lens_ReportFlags_hideEmptyItemFields . toLens _x
          toLens u = error $ ("Unexpected goal JSONText for ReportFlags: " ++ show u)
instance Path ReportFlags ReportFlags
    where type PathType ReportFlags
                        ReportFlags = Path_ReportFlags ReportFlags
          toLens _ = iso id id
instance Path ReportFlags String
    where type PathType ReportFlags String = Path_ReportFlags String
          toLens (Path_ReportFlags_hideEmptyItemFields _x) = lens_ReportFlags_hideEmptyItemFields . toLens _x
          toLens u = error $ ("Unexpected goal [Char] (aka String, aka FilePath, aka Checksum) for ReportFlags: " ++ show u)
instance Path ReportImage (Either URI ImageFile)
    where type PathType ReportImage
                        (Either URI ImageFile) = Path_ReportImage (Either URI ImageFile)
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal Either URI ImageFile for ReportImage: " ++ show u)
instance Path ReportImage (Maybe (Either URI ImageFile))
    where type PathType ReportImage
                        (Maybe (Either URI
                                       ImageFile)) = Path_ReportImage (Maybe (Either URI ImageFile))
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal Maybe (Either URI ImageFile) for ReportImage: " ++ show u)
instance Path ReportImage Bool
    where type PathType ReportImage Bool = Path_ReportImage Bool
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal Bool for ReportImage: " ++ show u)
instance Path ReportImage Dimension
    where type PathType ReportImage
                        Dimension = Path_ReportImage Dimension
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal Dimension for ReportImage: " ++ show u)
instance Path ReportImage Double
    where type PathType ReportImage Double = Path_ReportImage Double
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal Double for ReportImage: " ++ show u)
instance Path ReportImage ImageCrop
    where type PathType ReportImage
                        ImageCrop = Path_ReportImage ImageCrop
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal ImageCrop for ReportImage: " ++ show u)
instance Path ReportImage ImageFile
    where type PathType ReportImage
                        ImageFile = Path_ReportImage ImageFile
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal ImageFile for ReportImage: " ++ show u)
instance Path ReportImage ImageSize
    where type PathType ReportImage
                        ImageSize = Path_ReportImage ImageSize
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal ImageSize for ReportImage: " ++ show u)
instance Path ReportImage JSONText
    where type PathType ReportImage
                        JSONText = Path_ReportImage JSONText
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal JSONText for ReportImage: " ++ show u)
instance Path ReportImage Markup
    where type PathType ReportImage Markup = Path_ReportImage Markup
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal Markup for ReportImage: " ++ show u)
instance Path ReportImage MaybeImageFile
    where type PathType ReportImage
                        MaybeImageFile = Path_ReportImage MaybeImageFile
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal Maybe ImageFile (aka MaybeImageFile) for ReportImage: " ++ show u)
instance Path ReportImage ReportImage
    where type PathType ReportImage
                        ReportImage = Path_ReportImage ReportImage
          toLens _ = iso id id
instance Path ReportImage ReportImageView
    where type PathType ReportImage
                        ReportImageView = Path_ReportImage ReportImageView
          toLens (Path_ReportImage_View _) = viewLens :: Lens' ReportImage
                                                               ReportImageView
          toLens u = error $ ("Unexpected goal ReportImageView for ReportImage: " ++ show u)
instance Path ReportImage SaneSizeImageSize
    where type PathType ReportImage
                        SaneSizeImageSize = Path_ReportImage SaneSizeImageSize
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal SaneSize ImageSize (aka SaneSizeImageSize) for ReportImage: " ++ show u)
instance Path ReportImage String
    where type PathType ReportImage String = Path_ReportImage String
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal [Char] (aka String, aka FilePath, aka Checksum) for ReportImage: " ++ show u)
instance Path ReportImage Text
    where type PathType ReportImage Text = Path_ReportImage Text
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal Text for ReportImage: " ++ show u)
instance Path ReportImage URI
    where type PathType ReportImage URI = Path_ReportImage URI
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal URI for ReportImage: " ++ show u)
instance Path ReportImage Units
    where type PathType ReportImage Units = Path_ReportImage Units
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal Units for ReportImage: " ++ show u)
instance Path ReportImageView (Either URI ImageFile)
    where type PathType ReportImageView
                        (Either URI ImageFile) = Path_ReportImageView (Either URI
                                                                              ImageFile)
          toLens (Path_ReportImageView__picOriginal _x) = lens_ReportImageView__picOriginal . toLens _x
          toLens u = error $ ("Unexpected goal Either URI ImageFile for ReportImageView: " ++ show u)
instance Path ReportImageView (Maybe (Either URI ImageFile))
    where type PathType ReportImageView
                        (Maybe (Either URI
                                       ImageFile)) = Path_ReportImageView (Maybe (Either URI
                                                                                         ImageFile))
          toLens (Path_ReportImageView__picOriginal _x) = lens_ReportImageView__picOriginal
          toLens u = error $ ("Unexpected goal Maybe (Either URI ImageFile) for ReportImageView: " ++ show u)
instance Path ReportImageView Bool
    where type PathType ReportImageView
                        Bool = Path_ReportImageView Bool
          toLens (Path_ReportImageView__picMustEnlarge _x) = lens_ReportImageView__picMustEnlarge
          toLens u = error $ ("Unexpected goal Bool for ReportImageView: " ++ show u)
instance Path ReportImageView Dimension
    where type PathType ReportImageView
                        Dimension = Path_ReportImageView Dimension
          toLens (Path_ReportImageView__picSize _x) = lens_ReportImageView__picSize . toLens _x
          toLens u = error $ ("Unexpected goal Dimension for ReportImageView: " ++ show u)
instance Path ReportImageView Double
    where type PathType ReportImageView
                        Double = Path_ReportImageView Double
          toLens (Path_ReportImageView__picSize _x) = lens_ReportImageView__picSize . toLens _x
          toLens u = error $ ("Unexpected goal Double for ReportImageView: " ++ show u)
instance Path ReportImageView ImageCrop
    where type PathType ReportImageView
                        ImageCrop = Path_ReportImageView ImageCrop
          toLens (Path_ReportImageView__picCrop _x) = lens_ReportImageView__picCrop
          toLens u = error $ ("Unexpected goal ImageCrop for ReportImageView: " ++ show u)
instance Path ReportImageView ImageFile
    where type PathType ReportImageView
                        ImageFile = Path_ReportImageView ImageFile
          toLens (Path_ReportImageView__picOriginal _x) = lens_ReportImageView__picOriginal . toLens _x
          toLens u = error $ ("Unexpected goal ImageFile for ReportImageView: " ++ show u)
instance Path ReportImageView ImageSize
    where type PathType ReportImageView
                        ImageSize = Path_ReportImageView ImageSize
          toLens (Path_ReportImageView__picSize _x) = lens_ReportImageView__picSize . toLens _x
          toLens u = error $ ("Unexpected goal ImageSize for ReportImageView: " ++ show u)
instance Path ReportImageView JSONText
    where type PathType ReportImageView
                        JSONText = Path_ReportImageView JSONText
          toLens (Path_ReportImageView__picSize _x) = lens_ReportImageView__picSize . toLens _x
          toLens (Path_ReportImageView__picCaption _x) = lens_ReportImageView__picCaption . toLens _x
          toLens (Path_ReportImageView__picEditedDeprecated _x) = lens_ReportImageView__picEditedDeprecated . toLens _x
          toLens (Path_ReportImageView__picThumbDeprecated _x) = lens_ReportImageView__picThumbDeprecated . toLens _x
          toLens (Path_ReportImageView__picPrinterDeprecated _x) = lens_ReportImageView__picPrinterDeprecated . toLens _x
          toLens (Path_ReportImageView__picMustEnlarge _x) = lens_ReportImageView__picMustEnlarge . toLens _x
          toLens (Path_ReportImageView__picEnlargedDeprecated _x) = lens_ReportImageView__picEnlargedDeprecated . toLens _x
          toLens u = error $ ("Unexpected goal JSONText for ReportImageView: " ++ show u)
instance Path ReportImageView Markup
    where type PathType ReportImageView
                        Markup = Path_ReportImageView Markup
          toLens (Path_ReportImageView__picCaption _x) = lens_ReportImageView__picCaption
          toLens u = error $ ("Unexpected goal Markup for ReportImageView: " ++ show u)
instance Path ReportImageView MaybeImageFile
    where type PathType ReportImageView
                        MaybeImageFile = Path_ReportImageView MaybeImageFile
          toLens (Path_ReportImageView__picEditedDeprecated _x) = lens_ReportImageView__picEditedDeprecated
          toLens (Path_ReportImageView__picThumbDeprecated _x) = lens_ReportImageView__picThumbDeprecated
          toLens (Path_ReportImageView__picPrinterDeprecated _x) = lens_ReportImageView__picPrinterDeprecated
          toLens (Path_ReportImageView__picEnlargedDeprecated _x) = lens_ReportImageView__picEnlargedDeprecated
          toLens u = error $ ("Unexpected goal Maybe ImageFile (aka MaybeImageFile) for ReportImageView: " ++ show u)
instance Path ReportImageView ReportImageView
    where type PathType ReportImageView
                        ReportImageView = Path_ReportImageView ReportImageView
          toLens _ = iso id id
instance Path ReportImageView SaneSizeImageSize
    where type PathType ReportImageView
                        SaneSizeImageSize = Path_ReportImageView SaneSizeImageSize
          toLens (Path_ReportImageView__picSize _x) = lens_ReportImageView__picSize
          toLens u = error $ ("Unexpected goal SaneSize ImageSize (aka SaneSizeImageSize) for ReportImageView: " ++ show u)
instance Path ReportImageView String
    where type PathType ReportImageView
                        String = Path_ReportImageView String
          toLens (Path_ReportImageView__picSize _x) = lens_ReportImageView__picSize . toLens _x
          toLens (Path_ReportImageView__picEditedDeprecated _x) = lens_ReportImageView__picEditedDeprecated . toLens _x
          toLens (Path_ReportImageView__picThumbDeprecated _x) = lens_ReportImageView__picThumbDeprecated . toLens _x
          toLens (Path_ReportImageView__picPrinterDeprecated _x) = lens_ReportImageView__picPrinterDeprecated . toLens _x
          toLens (Path_ReportImageView__picMustEnlarge _x) = lens_ReportImageView__picMustEnlarge . toLens _x
          toLens (Path_ReportImageView__picEnlargedDeprecated _x) = lens_ReportImageView__picEnlargedDeprecated . toLens _x
          toLens u = error $ ("Unexpected goal [Char] (aka String, aka FilePath, aka Checksum) for ReportImageView: " ++ show u)
instance Path ReportImageView Text
    where type PathType ReportImageView
                        Text = Path_ReportImageView Text
          toLens (Path_ReportImageView__picCaption _x) = lens_ReportImageView__picCaption . toLens _x
          toLens u = error $ ("Unexpected goal Text for ReportImageView: " ++ show u)
instance Path ReportImageView URI
    where type PathType ReportImageView URI = Path_ReportImageView URI
          toLens (Path_ReportImageView__picOriginal _x) = lens_ReportImageView__picOriginal . toLens _x
          toLens u = error $ ("Unexpected goal URI for ReportImageView: " ++ show u)
instance Path ReportImageView Units
    where type PathType ReportImageView
                        Units = Path_ReportImageView Units
          toLens (Path_ReportImageView__picSize _x) = lens_ReportImageView__picSize . toLens _x
          toLens u = error $ ("Unexpected goal Units for ReportImageView: " ++ show u)
instance Path ReportImages (Either URI ImageFile)
    where type PathType ReportImages
                        (Either URI ImageFile) = Path_OMap ReportImageID
                                                           (Path_ReportImage (Either URI ImageFile))
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Either URI ImageFile for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance Path ReportImages (Maybe (Either URI ImageFile))
    where type PathType ReportImages
                        (Maybe (Either URI ImageFile)) = Path_OMap ReportImageID
                                                                   (Path_ReportImage (Maybe (Either URI
                                                                                                    ImageFile)))
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Maybe (Either URI ImageFile) for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance Path ReportImages Bool
    where type PathType ReportImages Bool = Path_OMap ReportImageID
                                                      (Path_ReportImage Bool)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Bool for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance Path ReportImages Dimension
    where type PathType ReportImages
                        Dimension = Path_OMap ReportImageID (Path_ReportImage Dimension)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Dimension for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance Path ReportImages Double
    where type PathType ReportImages Double = Path_OMap ReportImageID
                                                        (Path_ReportImage Double)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Double for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance Path ReportImages ImageCrop
    where type PathType ReportImages
                        ImageCrop = Path_OMap ReportImageID (Path_ReportImage ImageCrop)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal ImageCrop for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance Path ReportImages ImageFile
    where type PathType ReportImages
                        ImageFile = Path_OMap ReportImageID (Path_ReportImage ImageFile)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal ImageFile for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance Path ReportImages ImageSize
    where type PathType ReportImages
                        ImageSize = Path_OMap ReportImageID (Path_ReportImage ImageSize)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal ImageSize for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance Path ReportImages JSONText
    where type PathType ReportImages JSONText = Path_OMap ReportImageID
                                                          (Path_ReportImage JSONText)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance Path ReportImages Markup
    where type PathType ReportImages Markup = Path_OMap ReportImageID
                                                        (Path_ReportImage Markup)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Markup for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance Path ReportImages MaybeImageFile
    where type PathType ReportImages
                        MaybeImageFile = Path_OMap ReportImageID
                                                   (Path_ReportImage MaybeImageFile)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Maybe ImageFile (aka MaybeImageFile) for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance Path ReportImages ReportImage
    where type PathType ReportImages
                        ReportImage = Path_OMap ReportImageID
                                                (Path_ReportImage ReportImage)
          toLens (Path_At k _) = lens_omat k
          toLens u = error $ ("Unexpected goal ReportImage for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance Path ReportImages ReportImageView
    where type PathType ReportImages
                        ReportImageView = Path_OMap ReportImageID
                                                    (Path_ReportImage ReportImageView)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal ReportImageView for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance Path ReportImages ReportImages
    where type PathType ReportImages
                        ReportImages = Path_OMap ReportImageID
                                                 (Path_ReportImage ReportImages)
          toLens _ = iso id id
instance Path ReportImages SaneSizeImageSize
    where type PathType ReportImages
                        SaneSizeImageSize = Path_OMap ReportImageID
                                                      (Path_ReportImage SaneSizeImageSize)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal SaneSize ImageSize (aka SaneSizeImageSize) for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance Path ReportImages String
    where type PathType ReportImages String = Path_OMap ReportImageID
                                                        (Path_ReportImage String)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal [Char] (aka String, aka FilePath, aka Checksum) for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance Path ReportImages Text
    where type PathType ReportImages Text = Path_OMap ReportImageID
                                                      (Path_ReportImage Text)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Text for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance Path ReportImages URI
    where type PathType ReportImages URI = Path_OMap ReportImageID
                                                     (Path_ReportImage URI)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal URI for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance Path ReportImages Units
    where type PathType ReportImages Units = Path_OMap ReportImageID
                                                       (Path_ReportImage Units)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Units for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance Path ReportIntendedUse JSONText
    where type PathType ReportIntendedUse
                        JSONText = Path_ReportIntendedUse JSONText
          toLens (Path_ReportIntendedUse_View v) = (viewLens :: Lens' ReportIntendedUse
                                                                      String) . toLens v
          toLens u = error $ ("Unexpected goal JSONText for ReportIntendedUse: " ++ show u)
instance Path ReportIntendedUse ReportIntendedUse
    where type PathType ReportIntendedUse
                        ReportIntendedUse = Path_ReportIntendedUse ReportIntendedUse
          toLens _ = iso id id
instance Path ReportIntendedUse String
    where type PathType ReportIntendedUse
                        String = Path_ReportIntendedUse String
          toLens (Path_ReportIntendedUse_View _) = viewLens :: Lens' ReportIntendedUse
                                                                     String
          toLens u = error $ ("Unexpected goal [Char] (aka String, aka FilePath, aka Checksum) for ReportIntendedUse: " ++ show u)
instance Path ReportMap (Either URI ImageFile)
    where type PathType ReportMap
                        (Either URI ImageFile) = Path_ReportMap (Either URI ImageFile)
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Either URI ImageFile for ReportMap: " ++ show u)
instance Path ReportMap (Map ItemFieldName Markup)
    where type PathType ReportMap
                        (Map ItemFieldName Markup) = Path_ReportMap (Map ItemFieldName
                                                                         Markup)
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Map ItemFieldName Markup for ReportMap: " ++ show u)
instance Path ReportMap (Map ReportID Report)
    where type PathType ReportMap
                        (Map ReportID Report) = Path_ReportMap (Map ReportID Report)
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap
          toLens u = error $ ("Unexpected goal Map ReportID Report for ReportMap: " ++ show u)
instance Path ReportMap (Maybe (Either URI ImageFile))
    where type PathType ReportMap
                        (Maybe (Either URI ImageFile)) = Path_ReportMap (Maybe (Either URI
                                                                                       ImageFile))
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Maybe (Either URI ImageFile) for ReportMap: " ++ show u)
instance Path ReportMap AbbrevPair
    where type PathType ReportMap
                        AbbrevPair = Path_ReportMap AbbrevPair
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal (CIString, Markup) (aka AbbrevPair) for ReportMap: " ++ show u)
instance Path ReportMap AbbrevPairs
    where type PathType ReportMap
                        AbbrevPairs = Path_ReportMap AbbrevPairs
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Order AbbrevPairID ((CIString, Markup)) (aka AbbrevPairs) for ReportMap: " ++ show u)
instance Path ReportMap Author
    where type PathType ReportMap Author = Path_ReportMap Author
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Author for ReportMap: " ++ show u)
instance Path ReportMap Authors
    where type PathType ReportMap Authors = Path_ReportMap Authors
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Order AuthorID Author (aka Authors) for ReportMap: " ++ show u)
instance Path ReportMap Bool
    where type PathType ReportMap Bool = Path_ReportMap Bool
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Bool for ReportMap: " ++ show u)
instance Path ReportMap Branding
    where type PathType ReportMap Branding = Path_ReportMap Branding
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Branding for ReportMap: " ++ show u)
instance Path ReportMap CIString
    where type PathType ReportMap CIString = Path_ReportMap CIString
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal CIString for ReportMap: " ++ show u)
instance Path ReportMap Dimension
    where type PathType ReportMap Dimension = Path_ReportMap Dimension
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Dimension for ReportMap: " ++ show u)
instance Path ReportMap Double
    where type PathType ReportMap Double = Path_ReportMap Double
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Double for ReportMap: " ++ show u)
instance Path ReportMap ImageCrop
    where type PathType ReportMap ImageCrop = Path_ReportMap ImageCrop
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal ImageCrop for ReportMap: " ++ show u)
instance Path ReportMap ImageFile
    where type PathType ReportMap ImageFile = Path_ReportMap ImageFile
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal ImageFile for ReportMap: " ++ show u)
instance Path ReportMap ImageSize
    where type PathType ReportMap ImageSize = Path_ReportMap ImageSize
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal ImageSize for ReportMap: " ++ show u)
instance Path ReportMap Int64
    where type PathType ReportMap Int64 = Path_ReportMap Int64
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Int64 (aka EpochMilli) for ReportMap: " ++ show u)
instance Path ReportMap Integer
    where type PathType ReportMap Integer = Path_ReportMap Integer
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Integer for ReportMap: " ++ show u)
instance Path ReportMap Item
    where type PathType ReportMap Item = Path_ReportMap Item
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Item for ReportMap: " ++ show u)
instance Path ReportMap JSONText
    where type PathType ReportMap JSONText = Path_ReportMap JSONText
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal JSONText for ReportMap: " ++ show u)
instance Path ReportMap Markup
    where type PathType ReportMap Markup = Path_ReportMap Markup
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Markup for ReportMap: " ++ show u)
instance Path ReportMap MarkupPair
    where type PathType ReportMap
                        MarkupPair = Path_ReportMap MarkupPair
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal (Markup, Markup) (aka MarkupPair) for ReportMap: " ++ show u)
instance Path ReportMap MarkupPairs
    where type PathType ReportMap
                        MarkupPairs = Path_ReportMap MarkupPairs
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Order MarkupPairID ((Markup, Markup)) (aka MarkupPairs) for ReportMap: " ++ show u)
instance Path ReportMap Markups
    where type PathType ReportMap Markups = Path_ReportMap Markups
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Order MarkupID Markup (aka Markups) for ReportMap: " ++ show u)
instance Path ReportMap MaybeImageFile
    where type PathType ReportMap
                        MaybeImageFile = Path_ReportMap MaybeImageFile
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Maybe ImageFile (aka MaybeImageFile) for ReportMap: " ++ show u)
instance Path ReportMap MaybeReportIntendedUse
    where type PathType ReportMap
                        MaybeReportIntendedUse = Path_ReportMap MaybeReportIntendedUse
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Maybe ReportIntendedUse (aka MaybeReportIntendedUse) for ReportMap: " ++ show u)
instance Path ReportMap Permissions
    where type PathType ReportMap
                        Permissions = Path_ReportMap Permissions
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Permissions for ReportMap: " ++ show u)
instance Path ReportMap ReadOnlyFilePath
    where type PathType ReportMap
                        ReadOnlyFilePath = Path_ReportMap ReadOnlyFilePath
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal ReadOnly ([Char]) (aka ReadOnlyFilePath) for ReportMap: " ++ show u)
instance Path ReportMap Report
    where type PathType ReportMap Report = Path_ReportMap Report
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Report for ReportMap: " ++ show u)
instance Path ReportMap ReportElem
    where type PathType ReportMap
                        ReportElem = Path_ReportMap ReportElem
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal ReportElem for ReportMap: " ++ show u)
instance Path ReportMap ReportElems
    where type PathType ReportMap
                        ReportElems = Path_ReportMap ReportElems
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Order ReportElemID ReportElem (aka ReportElems) for ReportMap: " ++ show u)
instance Path ReportMap ReportFlags
    where type PathType ReportMap
                        ReportFlags = Path_ReportMap ReportFlags
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal ReportFlags for ReportMap: " ++ show u)
instance Path ReportMap ReportImage
    where type PathType ReportMap
                        ReportImage = Path_ReportMap ReportImage
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal ReportImage for ReportMap: " ++ show u)
instance Path ReportMap ReportImageView
    where type PathType ReportMap
                        ReportImageView = Path_ReportMap ReportImageView
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal ReportImageView for ReportMap: " ++ show u)
instance Path ReportMap ReportImages
    where type PathType ReportMap
                        ReportImages = Path_ReportMap ReportImages
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Order ReportImageID ReportImage (aka ReportImages) for ReportMap: " ++ show u)
instance Path ReportMap ReportMap
    where type PathType ReportMap ReportMap = Path_ReportMap ReportMap
          toLens _ = iso id id
instance Path ReportMap ReportStatus
    where type PathType ReportMap
                        ReportStatus = Path_ReportMap ReportStatus
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal ReportStatus for ReportMap: " ++ show u)
instance Path ReportMap ReportValueApproachInfo
    where type PathType ReportMap
                        ReportValueApproachInfo = Path_ReportMap ReportValueApproachInfo
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal ReportValueApproachInfo for ReportMap: " ++ show u)
instance Path ReportMap ReportValueTypeInfo
    where type PathType ReportMap
                        ReportValueTypeInfo = Path_ReportMap ReportValueTypeInfo
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal ReportValueTypeInfo for ReportMap: " ++ show u)
instance Path ReportMap ReportView
    where type PathType ReportMap
                        ReportView = Path_ReportMap ReportView
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal ReportView for ReportMap: " ++ show u)
instance Path ReportMap SaneSizeImageSize
    where type PathType ReportMap
                        SaneSizeImageSize = Path_ReportMap SaneSizeImageSize
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal SaneSize ImageSize (aka SaneSizeImageSize) for ReportMap: " ++ show u)
instance Path ReportMap String
    where type PathType ReportMap String = Path_ReportMap String
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal [Char] (aka String, aka FilePath, aka Checksum) for ReportMap: " ++ show u)
instance Path ReportMap Text
    where type PathType ReportMap Text = Path_ReportMap Text
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Text for ReportMap: " ++ show u)
instance Path ReportMap URI
    where type PathType ReportMap URI = Path_ReportMap URI
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal URI for ReportMap: " ++ show u)
instance Path ReportMap UUID
    where type PathType ReportMap UUID = Path_ReportMap UUID
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal UUID for ReportMap: " ++ show u)
instance Path ReportMap Units
    where type PathType ReportMap Units = Path_ReportMap Units
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Units for ReportMap: " ++ show u)
instance Path ReportMap UserId
    where type PathType ReportMap UserId = Path_ReportMap UserId
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal UserId for ReportMap: " ++ show u)
instance Path ReportMap UserIds
    where type PathType ReportMap UserIds = Path_ReportMap UserIds
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal [UserId] (aka UserIds) for ReportMap: " ++ show u)
instance Path ReportStatus JSONText
    where type PathType ReportStatus
                        JSONText = Path_ReportStatus JSONText
          toLens (Path_ReportStatus_View v) = (viewLens :: Lens' ReportStatus
                                                                 String) . toLens v
          toLens u = error $ ("Unexpected goal JSONText for ReportStatus: " ++ show u)
instance Path ReportStatus ReportStatus
    where type PathType ReportStatus
                        ReportStatus = Path_ReportStatus ReportStatus
          toLens _ = iso id id
instance Path ReportStatus String
    where type PathType ReportStatus String = Path_ReportStatus String
          toLens (Path_ReportStatus_View _) = viewLens :: Lens' ReportStatus
                                                                String
          toLens u = error $ ("Unexpected goal [Char] (aka String, aka FilePath, aka Checksum) for ReportStatus: " ++ show u)
instance Path ReportValueApproachInfo JSONText
    where type PathType ReportValueApproachInfo
                        JSONText = Path_ReportValueApproachInfo JSONText
          toLens (Path_ReportValueApproachInfo_reportValueApproachName _x) = lens_ReportValueApproachInfo_reportValueApproachName . toLens _x
          toLens (Path_ReportValueApproachInfo_reportValueApproachDescription _x) = lens_ReportValueApproachInfo_reportValueApproachDescription . toLens _x
          toLens u = error $ ("Unexpected goal JSONText for ReportValueApproachInfo: " ++ show u)
instance Path ReportValueApproachInfo Markup
    where type PathType ReportValueApproachInfo
                        Markup = Path_ReportValueApproachInfo Markup
          toLens (Path_ReportValueApproachInfo_reportValueApproachName _x) = lens_ReportValueApproachInfo_reportValueApproachName
          toLens (Path_ReportValueApproachInfo_reportValueApproachDescription _x) = lens_ReportValueApproachInfo_reportValueApproachDescription
          toLens u = error $ ("Unexpected goal Markup for ReportValueApproachInfo: " ++ show u)
instance Path ReportValueApproachInfo ReportValueApproachInfo
    where type PathType ReportValueApproachInfo
                        ReportValueApproachInfo = Path_ReportValueApproachInfo ReportValueApproachInfo
          toLens _ = iso id id
instance Path ReportValueApproachInfo Text
    where type PathType ReportValueApproachInfo
                        Text = Path_ReportValueApproachInfo Text
          toLens (Path_ReportValueApproachInfo_reportValueApproachName _x) = lens_ReportValueApproachInfo_reportValueApproachName . toLens _x
          toLens (Path_ReportValueApproachInfo_reportValueApproachDescription _x) = lens_ReportValueApproachInfo_reportValueApproachDescription . toLens _x
          toLens u = error $ ("Unexpected goal Text for ReportValueApproachInfo: " ++ show u)
instance Path ReportValueTypeInfo JSONText
    where type PathType ReportValueTypeInfo
                        JSONText = Path_ReportValueTypeInfo JSONText
          toLens (Path_ReportValueTypeInfo_reportValueTypeName _x) = lens_ReportValueTypeInfo_reportValueTypeName . toLens _x
          toLens (Path_ReportValueTypeInfo_reportValueTypeDescription _x) = lens_ReportValueTypeInfo_reportValueTypeDescription . toLens _x
          toLens (Path_ReportValueTypeInfo_reportValueTypeDefinition _x) = lens_ReportValueTypeInfo_reportValueTypeDefinition . toLens _x
          toLens u = error $ ("Unexpected goal JSONText for ReportValueTypeInfo: " ++ show u)
instance Path ReportValueTypeInfo Markup
    where type PathType ReportValueTypeInfo
                        Markup = Path_ReportValueTypeInfo Markup
          toLens (Path_ReportValueTypeInfo_reportValueTypeName _x) = lens_ReportValueTypeInfo_reportValueTypeName
          toLens (Path_ReportValueTypeInfo_reportValueTypeDescription _x) = lens_ReportValueTypeInfo_reportValueTypeDescription
          toLens (Path_ReportValueTypeInfo_reportValueTypeDefinition _x) = lens_ReportValueTypeInfo_reportValueTypeDefinition
          toLens u = error $ ("Unexpected goal Markup for ReportValueTypeInfo: " ++ show u)
instance Path ReportValueTypeInfo ReportValueTypeInfo
    where type PathType ReportValueTypeInfo
                        ReportValueTypeInfo = Path_ReportValueTypeInfo ReportValueTypeInfo
          toLens _ = iso id id
instance Path ReportValueTypeInfo Text
    where type PathType ReportValueTypeInfo
                        Text = Path_ReportValueTypeInfo Text
          toLens (Path_ReportValueTypeInfo_reportValueTypeName _x) = lens_ReportValueTypeInfo_reportValueTypeName . toLens _x
          toLens (Path_ReportValueTypeInfo_reportValueTypeDescription _x) = lens_ReportValueTypeInfo_reportValueTypeDescription . toLens _x
          toLens (Path_ReportValueTypeInfo_reportValueTypeDefinition _x) = lens_ReportValueTypeInfo_reportValueTypeDefinition . toLens _x
          toLens u = error $ ("Unexpected goal Text for ReportValueTypeInfo: " ++ show u)
instance Path ReportView (Either URI ImageFile)
    where type PathType ReportView
                        (Either URI ImageFile) = Path_ReportView (Either URI ImageFile)
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal Either URI ImageFile for ReportView: " ++ show u)
instance Path ReportView (Map ItemFieldName Markup)
    where type PathType ReportView
                        (Map ItemFieldName Markup) = Path_ReportView (Map ItemFieldName
                                                                          Markup)
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal Map ItemFieldName Markup for ReportView: " ++ show u)
instance Path ReportView (Maybe (Either URI ImageFile))
    where type PathType ReportView
                        (Maybe (Either URI ImageFile)) = Path_ReportView (Maybe (Either URI
                                                                                        ImageFile))
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal Maybe (Either URI ImageFile) for ReportView: " ++ show u)
instance Path ReportView AbbrevPair
    where type PathType ReportView
                        AbbrevPair = Path_ReportView AbbrevPair
          toLens (Path_ReportView__reportAbbrevs _x) = lens_ReportView__reportAbbrevs . toLens _x
          toLens u = error $ ("Unexpected goal (CIString, Markup) (aka AbbrevPair) for ReportView: " ++ show u)
instance Path ReportView AbbrevPairs
    where type PathType ReportView
                        AbbrevPairs = Path_ReportView AbbrevPairs
          toLens (Path_ReportView__reportAbbrevs _x) = lens_ReportView__reportAbbrevs
          toLens u = error $ ("Unexpected goal Order AbbrevPairID ((CIString, Markup)) (aka AbbrevPairs) for ReportView: " ++ show u)
instance Path ReportView Author
    where type PathType ReportView Author = Path_ReportView Author
          toLens (Path_ReportView__reportAuthors _x) = lens_ReportView__reportAuthors . toLens _x
          toLens u = error $ ("Unexpected goal Author for ReportView: " ++ show u)
instance Path ReportView Authors
    where type PathType ReportView Authors = Path_ReportView Authors
          toLens (Path_ReportView__reportAuthors _x) = lens_ReportView__reportAuthors
          toLens u = error $ ("Unexpected goal Order AuthorID Author (aka Authors) for ReportView: " ++ show u)
instance Path ReportView Bool
    where type PathType ReportView Bool = Path_ReportView Bool
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens (Path_ReportView__reportRedacted _x) = lens_ReportView__reportRedacted
          toLens (Path_ReportView__reportFlags _x) = lens_ReportView__reportFlags . toLens _x
          toLens (Path_ReportView__reportOrderByItemName _x) = lens_ReportView__reportOrderByItemName
          toLens (Path_ReportView__reportDisplayItemName _x) = lens_ReportView__reportDisplayItemName
          toLens u = error $ ("Unexpected goal Bool for ReportView: " ++ show u)
instance Path ReportView Branding
    where type PathType ReportView Branding = Path_ReportView Branding
          toLens (Path_ReportView__reportBranding _x) = lens_ReportView__reportBranding
          toLens u = error $ ("Unexpected goal Branding for ReportView: " ++ show u)
instance Path ReportView CIString
    where type PathType ReportView CIString = Path_ReportView CIString
          toLens (Path_ReportView__reportAbbrevs _x) = lens_ReportView__reportAbbrevs . toLens _x
          toLens u = error $ ("Unexpected goal CIString for ReportView: " ++ show u)
instance Path ReportView Dimension
    where type PathType ReportView
                        Dimension = Path_ReportView Dimension
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal Dimension for ReportView: " ++ show u)
instance Path ReportView Double
    where type PathType ReportView Double = Path_ReportView Double
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal Double for ReportView: " ++ show u)
instance Path ReportView ImageCrop
    where type PathType ReportView
                        ImageCrop = Path_ReportView ImageCrop
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal ImageCrop for ReportView: " ++ show u)
instance Path ReportView ImageFile
    where type PathType ReportView
                        ImageFile = Path_ReportView ImageFile
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal ImageFile for ReportView: " ++ show u)
instance Path ReportView ImageSize
    where type PathType ReportView
                        ImageSize = Path_ReportView ImageSize
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal ImageSize for ReportView: " ++ show u)
instance Path ReportView Int64
    where type PathType ReportView Int64 = Path_ReportView Int64
          toLens (Path_ReportView__reportCreated _x) = lens_ReportView__reportCreated
          toLens u = error $ ("Unexpected goal Int64 (aka EpochMilli) for ReportView: " ++ show u)
instance Path ReportView Integer
    where type PathType ReportView Integer = Path_ReportView Integer
          toLens (Path_ReportView__reportRevision _x) = lens_ReportView__reportRevision
          toLens u = error $ ("Unexpected goal Integer for ReportView: " ++ show u)
instance Path ReportView Item
    where type PathType ReportView Item = Path_ReportView Item
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal Item for ReportView: " ++ show u)
instance Path ReportView JSONText
    where type PathType ReportView JSONText = Path_ReportView JSONText
          toLens (Path_ReportView__reportName _x) = lens_ReportView__reportName . toLens _x
          toLens (Path_ReportView__reportDate _x) = lens_ReportView__reportDate . toLens _x
          toLens (Path_ReportView__reportContractDate _x) = lens_ReportView__reportContractDate . toLens _x
          toLens (Path_ReportView__reportInspectionDate _x) = lens_ReportView__reportInspectionDate . toLens _x
          toLens (Path_ReportView__reportEffectiveDate _x) = lens_ReportView__reportEffectiveDate . toLens _x
          toLens (Path_ReportView__reportAuthors _x) = lens_ReportView__reportAuthors . toLens _x
          toLens (Path_ReportView__reportPreparer _x) = lens_ReportView__reportPreparer . toLens _x
          toLens (Path_ReportView__reportPreparerEIN _x) = lens_ReportView__reportPreparerEIN . toLens _x
          toLens (Path_ReportView__reportPreparerAddress _x) = lens_ReportView__reportPreparerAddress . toLens _x
          toLens (Path_ReportView__reportPreparerEMail _x) = lens_ReportView__reportPreparerEMail . toLens _x
          toLens (Path_ReportView__reportPreparerWebsite _x) = lens_ReportView__reportPreparerWebsite . toLens _x
          toLens (Path_ReportView__reportAbbrevs _x) = lens_ReportView__reportAbbrevs . toLens _x
          toLens (Path_ReportView__reportTitle _x) = lens_ReportView__reportTitle . toLens _x
          toLens (Path_ReportView__reportHeader _x) = lens_ReportView__reportHeader . toLens _x
          toLens (Path_ReportView__reportFooter _x) = lens_ReportView__reportFooter . toLens _x
          toLens (Path_ReportView__reportIntendedUse _x) = lens_ReportView__reportIntendedUse . toLens _x
          toLens (Path_ReportView__reportValueTypeInfo _x) = lens_ReportView__reportValueTypeInfo . toLens _x
          toLens (Path_ReportView__reportValueApproachInfo _x) = lens_ReportView__reportValueApproachInfo . toLens _x
          toLens (Path_ReportView__reportClientName _x) = lens_ReportView__reportClientName . toLens _x
          toLens (Path_ReportView__reportClientAddress _x) = lens_ReportView__reportClientAddress . toLens _x
          toLens (Path_ReportView__reportClientGreeting _x) = lens_ReportView__reportClientGreeting . toLens _x
          toLens (Path_ReportView__reportItemsOwnerFull _x) = lens_ReportView__reportItemsOwnerFull . toLens _x
          toLens (Path_ReportView__reportItemsOwner _x) = lens_ReportView__reportItemsOwner . toLens _x
          toLens (Path_ReportView__reportBriefItems _x) = lens_ReportView__reportBriefItems . toLens _x
          toLens (Path_ReportView__reportInspectionLocation _x) = lens_ReportView__reportInspectionLocation . toLens _x
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens (Path_ReportView__reportGlossary _x) = lens_ReportView__reportGlossary . toLens _x
          toLens (Path_ReportView__reportSources _x) = lens_ReportView__reportSources . toLens _x
          toLens (Path_ReportView__reportLetterOfTransmittal _x) = lens_ReportView__reportLetterOfTransmittal . toLens _x
          toLens (Path_ReportView__reportScopeOfWork _x) = lens_ReportView__reportScopeOfWork . toLens _x
          toLens (Path_ReportView__reportCertification _x) = lens_ReportView__reportCertification . toLens _x
          toLens (Path_ReportView__reportLimitingConditions _x) = lens_ReportView__reportLimitingConditions . toLens _x
          toLens (Path_ReportView__reportPrivacyPolicy _x) = lens_ReportView__reportPrivacyPolicy . toLens _x
          toLens (Path_ReportView__reportPerms _x) = lens_ReportView__reportPerms . toLens _x
          toLens (Path_ReportView__reportBranding _x) = lens_ReportView__reportBranding . toLens _x
          toLens (Path_ReportView__reportStatus _x) = lens_ReportView__reportStatus . toLens _x
          toLens (Path_ReportView__reportRedacted _x) = lens_ReportView__reportRedacted . toLens _x
          toLens (Path_ReportView__reportFlags _x) = lens_ReportView__reportFlags . toLens _x
          toLens (Path_ReportView__reportOrderByItemName _x) = lens_ReportView__reportOrderByItemName . toLens _x
          toLens (Path_ReportView__reportDisplayItemName _x) = lens_ReportView__reportDisplayItemName . toLens _x
          toLens u = error $ ("Unexpected goal JSONText for ReportView: " ++ show u)
instance Path ReportView Markup
    where type PathType ReportView Markup = Path_ReportView Markup
          toLens (Path_ReportView__reportName _x) = lens_ReportView__reportName
          toLens (Path_ReportView__reportDate _x) = lens_ReportView__reportDate
          toLens (Path_ReportView__reportContractDate _x) = lens_ReportView__reportContractDate
          toLens (Path_ReportView__reportInspectionDate _x) = lens_ReportView__reportInspectionDate
          toLens (Path_ReportView__reportEffectiveDate _x) = lens_ReportView__reportEffectiveDate
          toLens (Path_ReportView__reportAuthors _x) = lens_ReportView__reportAuthors . toLens _x
          toLens (Path_ReportView__reportPreparer _x) = lens_ReportView__reportPreparer
          toLens (Path_ReportView__reportPreparerEIN _x) = lens_ReportView__reportPreparerEIN
          toLens (Path_ReportView__reportPreparerAddress _x) = lens_ReportView__reportPreparerAddress
          toLens (Path_ReportView__reportPreparerEMail _x) = lens_ReportView__reportPreparerEMail
          toLens (Path_ReportView__reportPreparerWebsite _x) = lens_ReportView__reportPreparerWebsite
          toLens (Path_ReportView__reportAbbrevs _x) = lens_ReportView__reportAbbrevs . toLens _x
          toLens (Path_ReportView__reportTitle _x) = lens_ReportView__reportTitle
          toLens (Path_ReportView__reportHeader _x) = lens_ReportView__reportHeader
          toLens (Path_ReportView__reportFooter _x) = lens_ReportView__reportFooter
          toLens (Path_ReportView__reportValueTypeInfo _x) = lens_ReportView__reportValueTypeInfo . toLens _x
          toLens (Path_ReportView__reportValueApproachInfo _x) = lens_ReportView__reportValueApproachInfo . toLens _x
          toLens (Path_ReportView__reportClientName _x) = lens_ReportView__reportClientName
          toLens (Path_ReportView__reportClientAddress _x) = lens_ReportView__reportClientAddress
          toLens (Path_ReportView__reportClientGreeting _x) = lens_ReportView__reportClientGreeting
          toLens (Path_ReportView__reportItemsOwnerFull _x) = lens_ReportView__reportItemsOwnerFull
          toLens (Path_ReportView__reportItemsOwner _x) = lens_ReportView__reportItemsOwner
          toLens (Path_ReportView__reportBriefItems _x) = lens_ReportView__reportBriefItems
          toLens (Path_ReportView__reportInspectionLocation _x) = lens_ReportView__reportInspectionLocation
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens (Path_ReportView__reportGlossary _x) = lens_ReportView__reportGlossary . toLens _x
          toLens (Path_ReportView__reportSources _x) = lens_ReportView__reportSources . toLens _x
          toLens (Path_ReportView__reportLetterOfTransmittal _x) = lens_ReportView__reportLetterOfTransmittal
          toLens (Path_ReportView__reportScopeOfWork _x) = lens_ReportView__reportScopeOfWork
          toLens (Path_ReportView__reportCertification _x) = lens_ReportView__reportCertification . toLens _x
          toLens (Path_ReportView__reportLimitingConditions _x) = lens_ReportView__reportLimitingConditions . toLens _x
          toLens (Path_ReportView__reportPrivacyPolicy _x) = lens_ReportView__reportPrivacyPolicy
          toLens u = error $ ("Unexpected goal Markup for ReportView: " ++ show u)
instance Path ReportView MarkupPair
    where type PathType ReportView
                        MarkupPair = Path_ReportView MarkupPair
          toLens (Path_ReportView__reportGlossary _x) = lens_ReportView__reportGlossary . toLens _x
          toLens (Path_ReportView__reportSources _x) = lens_ReportView__reportSources . toLens _x
          toLens u = error $ ("Unexpected goal (Markup, Markup) (aka MarkupPair) for ReportView: " ++ show u)
instance Path ReportView MarkupPairs
    where type PathType ReportView
                        MarkupPairs = Path_ReportView MarkupPairs
          toLens (Path_ReportView__reportGlossary _x) = lens_ReportView__reportGlossary
          toLens (Path_ReportView__reportSources _x) = lens_ReportView__reportSources
          toLens u = error $ ("Unexpected goal Order MarkupPairID ((Markup, Markup)) (aka MarkupPairs) for ReportView: " ++ show u)
instance Path ReportView Markups
    where type PathType ReportView Markups = Path_ReportView Markups
          toLens (Path_ReportView__reportCertification _x) = lens_ReportView__reportCertification
          toLens (Path_ReportView__reportLimitingConditions _x) = lens_ReportView__reportLimitingConditions
          toLens u = error $ ("Unexpected goal Order MarkupID Markup (aka Markups) for ReportView: " ++ show u)
instance Path ReportView MaybeImageFile
    where type PathType ReportView
                        MaybeImageFile = Path_ReportView MaybeImageFile
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal Maybe ImageFile (aka MaybeImageFile) for ReportView: " ++ show u)
instance Path ReportView MaybeReportIntendedUse
    where type PathType ReportView
                        MaybeReportIntendedUse = Path_ReportView MaybeReportIntendedUse
          toLens (Path_ReportView__reportIntendedUse _x) = lens_ReportView__reportIntendedUse
          toLens u = error $ ("Unexpected goal Maybe ReportIntendedUse (aka MaybeReportIntendedUse) for ReportView: " ++ show u)
instance Path ReportView Permissions
    where type PathType ReportView
                        Permissions = Path_ReportView Permissions
          toLens (Path_ReportView__reportPerms _x) = lens_ReportView__reportPerms
          toLens u = error $ ("Unexpected goal Permissions for ReportView: " ++ show u)
instance Path ReportView ReadOnlyFilePath
    where type PathType ReportView
                        ReadOnlyFilePath = Path_ReportView ReadOnlyFilePath
          toLens (Path_ReportView__reportFolder _x) = lens_ReportView__reportFolder
          toLens u = error $ ("Unexpected goal ReadOnly ([Char]) (aka ReadOnlyFilePath) for ReportView: " ++ show u)
instance Path ReportView ReportElem
    where type PathType ReportView
                        ReportElem = Path_ReportView ReportElem
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal ReportElem for ReportView: " ++ show u)
instance Path ReportView ReportElems
    where type PathType ReportView
                        ReportElems = Path_ReportView ReportElems
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody
          toLens u = error $ ("Unexpected goal Order ReportElemID ReportElem (aka ReportElems) for ReportView: " ++ show u)
instance Path ReportView ReportFlags
    where type PathType ReportView
                        ReportFlags = Path_ReportView ReportFlags
          toLens (Path_ReportView__reportFlags _x) = lens_ReportView__reportFlags
          toLens u = error $ ("Unexpected goal ReportFlags for ReportView: " ++ show u)
instance Path ReportView ReportImage
    where type PathType ReportView
                        ReportImage = Path_ReportView ReportImage
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal ReportImage for ReportView: " ++ show u)
instance Path ReportView ReportImageView
    where type PathType ReportView
                        ReportImageView = Path_ReportView ReportImageView
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal ReportImageView for ReportView: " ++ show u)
instance Path ReportView ReportImages
    where type PathType ReportView
                        ReportImages = Path_ReportView ReportImages
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal Order ReportImageID ReportImage (aka ReportImages) for ReportView: " ++ show u)
instance Path ReportView ReportStatus
    where type PathType ReportView
                        ReportStatus = Path_ReportView ReportStatus
          toLens (Path_ReportView__reportStatus _x) = lens_ReportView__reportStatus
          toLens u = error $ ("Unexpected goal ReportStatus for ReportView: " ++ show u)
instance Path ReportView ReportValueApproachInfo
    where type PathType ReportView
                        ReportValueApproachInfo = Path_ReportView ReportValueApproachInfo
          toLens (Path_ReportView__reportValueApproachInfo _x) = lens_ReportView__reportValueApproachInfo
          toLens u = error $ ("Unexpected goal ReportValueApproachInfo for ReportView: " ++ show u)
instance Path ReportView ReportValueTypeInfo
    where type PathType ReportView
                        ReportValueTypeInfo = Path_ReportView ReportValueTypeInfo
          toLens (Path_ReportView__reportValueTypeInfo _x) = lens_ReportView__reportValueTypeInfo
          toLens u = error $ ("Unexpected goal ReportValueTypeInfo for ReportView: " ++ show u)
instance Path ReportView ReportView
    where type PathType ReportView
                        ReportView = Path_ReportView ReportView
          toLens _ = iso id id
instance Path ReportView SaneSizeImageSize
    where type PathType ReportView
                        SaneSizeImageSize = Path_ReportView SaneSizeImageSize
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal SaneSize ImageSize (aka SaneSizeImageSize) for ReportView: " ++ show u)
instance Path ReportView String
    where type PathType ReportView String = Path_ReportView String
          toLens (Path_ReportView__reportIntendedUse _x) = lens_ReportView__reportIntendedUse . toLens _x
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens (Path_ReportView__reportStatus _x) = lens_ReportView__reportStatus . toLens _x
          toLens (Path_ReportView__reportRedacted _x) = lens_ReportView__reportRedacted . toLens _x
          toLens (Path_ReportView__reportFlags _x) = lens_ReportView__reportFlags . toLens _x
          toLens (Path_ReportView__reportOrderByItemName _x) = lens_ReportView__reportOrderByItemName . toLens _x
          toLens (Path_ReportView__reportDisplayItemName _x) = lens_ReportView__reportDisplayItemName . toLens _x
          toLens u = error $ ("Unexpected goal [Char] (aka String, aka FilePath, aka Checksum) for ReportView: " ++ show u)
instance Path ReportView Text
    where type PathType ReportView Text = Path_ReportView Text
          toLens (Path_ReportView__reportName _x) = lens_ReportView__reportName . toLens _x
          toLens (Path_ReportView__reportDate _x) = lens_ReportView__reportDate . toLens _x
          toLens (Path_ReportView__reportContractDate _x) = lens_ReportView__reportContractDate . toLens _x
          toLens (Path_ReportView__reportInspectionDate _x) = lens_ReportView__reportInspectionDate . toLens _x
          toLens (Path_ReportView__reportEffectiveDate _x) = lens_ReportView__reportEffectiveDate . toLens _x
          toLens (Path_ReportView__reportAuthors _x) = lens_ReportView__reportAuthors . toLens _x
          toLens (Path_ReportView__reportPreparer _x) = lens_ReportView__reportPreparer . toLens _x
          toLens (Path_ReportView__reportPreparerEIN _x) = lens_ReportView__reportPreparerEIN . toLens _x
          toLens (Path_ReportView__reportPreparerAddress _x) = lens_ReportView__reportPreparerAddress . toLens _x
          toLens (Path_ReportView__reportPreparerEMail _x) = lens_ReportView__reportPreparerEMail . toLens _x
          toLens (Path_ReportView__reportPreparerWebsite _x) = lens_ReportView__reportPreparerWebsite . toLens _x
          toLens (Path_ReportView__reportAbbrevs _x) = lens_ReportView__reportAbbrevs . toLens _x
          toLens (Path_ReportView__reportTitle _x) = lens_ReportView__reportTitle . toLens _x
          toLens (Path_ReportView__reportHeader _x) = lens_ReportView__reportHeader . toLens _x
          toLens (Path_ReportView__reportFooter _x) = lens_ReportView__reportFooter . toLens _x
          toLens (Path_ReportView__reportValueTypeInfo _x) = lens_ReportView__reportValueTypeInfo . toLens _x
          toLens (Path_ReportView__reportValueApproachInfo _x) = lens_ReportView__reportValueApproachInfo . toLens _x
          toLens (Path_ReportView__reportClientName _x) = lens_ReportView__reportClientName . toLens _x
          toLens (Path_ReportView__reportClientAddress _x) = lens_ReportView__reportClientAddress . toLens _x
          toLens (Path_ReportView__reportClientGreeting _x) = lens_ReportView__reportClientGreeting . toLens _x
          toLens (Path_ReportView__reportItemsOwnerFull _x) = lens_ReportView__reportItemsOwnerFull . toLens _x
          toLens (Path_ReportView__reportItemsOwner _x) = lens_ReportView__reportItemsOwner . toLens _x
          toLens (Path_ReportView__reportBriefItems _x) = lens_ReportView__reportBriefItems . toLens _x
          toLens (Path_ReportView__reportInspectionLocation _x) = lens_ReportView__reportInspectionLocation . toLens _x
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens (Path_ReportView__reportGlossary _x) = lens_ReportView__reportGlossary . toLens _x
          toLens (Path_ReportView__reportSources _x) = lens_ReportView__reportSources . toLens _x
          toLens (Path_ReportView__reportLetterOfTransmittal _x) = lens_ReportView__reportLetterOfTransmittal . toLens _x
          toLens (Path_ReportView__reportScopeOfWork _x) = lens_ReportView__reportScopeOfWork . toLens _x
          toLens (Path_ReportView__reportCertification _x) = lens_ReportView__reportCertification . toLens _x
          toLens (Path_ReportView__reportLimitingConditions _x) = lens_ReportView__reportLimitingConditions . toLens _x
          toLens (Path_ReportView__reportPrivacyPolicy _x) = lens_ReportView__reportPrivacyPolicy . toLens _x
          toLens (Path_ReportView__reportPerms _x) = lens_ReportView__reportPerms . toLens _x
          toLens (Path_ReportView__reportBranding _x) = lens_ReportView__reportBranding . toLens _x
          toLens u = error $ ("Unexpected goal Text for ReportView: " ++ show u)
instance Path ReportView URI
    where type PathType ReportView URI = Path_ReportView URI
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal URI for ReportView: " ++ show u)
instance Path ReportView UUID
    where type PathType ReportView UUID = Path_ReportView UUID
          toLens (Path_ReportView__reportUUID _x) = lens_ReportView__reportUUID
          toLens u = error $ ("Unexpected goal UUID for ReportView: " ++ show u)
instance Path ReportView Units
    where type PathType ReportView Units = Path_ReportView Units
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal Units for ReportView: " ++ show u)
instance Path ReportView UserId
    where type PathType ReportView UserId = Path_ReportView UserId
          toLens (Path_ReportView__reportPerms _x) = lens_ReportView__reportPerms . toLens _x
          toLens u = error $ ("Unexpected goal UserId for ReportView: " ++ show u)
instance Path ReportView UserIds
    where type PathType ReportView UserIds = Path_ReportView UserIds
          toLens (Path_ReportView__reportPerms _x) = lens_ReportView__reportPerms . toLens _x
          toLens u = error $ ("Unexpected goal [UserId] (aka UserIds) for ReportView: " ++ show u)
instance Path SaneSizeImageSize Dimension
    where type PathType SaneSizeImageSize
                        Dimension = Path_SaneSizeImageSize Dimension
          toLens (Path_SaneSizeImageSize_View v) = (viewLens :: Lens' (SaneSize ImageSize)
                                                                      ImageSize) . toLens v
          toLens u = error $ ("Unexpected goal Dimension for SaneSize ImageSize (aka SaneSizeImageSize): " ++ show u)
instance Path SaneSizeImageSize Double
    where type PathType SaneSizeImageSize
                        Double = Path_SaneSizeImageSize Double
          toLens (Path_SaneSizeImageSize_View v) = (viewLens :: Lens' (SaneSize ImageSize)
                                                                      ImageSize) . toLens v
          toLens u = error $ ("Unexpected goal Double for SaneSize ImageSize (aka SaneSizeImageSize): " ++ show u)
instance Path SaneSizeImageSize ImageSize
    where type PathType SaneSizeImageSize
                        ImageSize = Path_SaneSizeImageSize ImageSize
          toLens (Path_SaneSizeImageSize_View _) = viewLens :: Lens' (SaneSize ImageSize)
                                                                     ImageSize
          toLens u = error $ ("Unexpected goal ImageSize for SaneSize ImageSize (aka SaneSizeImageSize): " ++ show u)
instance Path SaneSizeImageSize JSONText
    where type PathType SaneSizeImageSize
                        JSONText = Path_SaneSizeImageSize JSONText
          toLens (Path_SaneSizeImageSize_View v) = (viewLens :: Lens' (SaneSize ImageSize)
                                                                      ImageSize) . toLens v
          toLens u = error $ ("Unexpected goal JSONText for SaneSize ImageSize (aka SaneSizeImageSize): " ++ show u)
instance Path SaneSizeImageSize SaneSizeImageSize
    where type PathType SaneSizeImageSize
                        SaneSizeImageSize = Path_SaneSizeImageSize SaneSizeImageSize
          toLens _ = iso id id
instance Path SaneSizeImageSize String
    where type PathType SaneSizeImageSize
                        String = Path_SaneSizeImageSize String
          toLens (Path_SaneSizeImageSize_View v) = (viewLens :: Lens' (SaneSize ImageSize)
                                                                      ImageSize) . toLens v
          toLens u = error $ ("Unexpected goal [Char] (aka String, aka FilePath, aka Checksum) for SaneSize ImageSize (aka SaneSizeImageSize): " ++ show u)
instance Path SaneSizeImageSize Units
    where type PathType SaneSizeImageSize
                        Units = Path_SaneSizeImageSize Units
          toLens (Path_SaneSizeImageSize_View v) = (viewLens :: Lens' (SaneSize ImageSize)
                                                                      ImageSize) . toLens v
          toLens u = error $ ("Unexpected goal Units for SaneSize ImageSize (aka SaneSizeImageSize): " ++ show u)
instance Path String JSONText
    where type PathType String JSONText = Path_String JSONText
          toLens (Path_String_View _) = viewLens :: Lens' ([Char]) JSONText
          toLens u = error $ ("Unexpected goal JSONText for [Char] (aka String, aka FilePath, aka Checksum): " ++ show u)
instance Path String String
    where type PathType String String = Path_String String
          toLens _ = iso id id
instance Path Text JSONText
    where type PathType Text JSONText = Path_Text JSONText
          toLens (Path_Text_View _) = viewLens :: Lens' Text JSONText
          toLens u = error $ ("Unexpected goal JSONText for Text: " ++ show u)
instance Path Text Text
    where type PathType Text Text = Path_Text Text
          toLens _ = iso id id
instance Path URI URI
    where type PathType URI URI = Path_URI URI
          toLens _ = iso id id
instance Path UUID UUID
    where type PathType UUID UUID = Path_UUID UUID
          toLens _ = iso id id
instance Path Units JSONText
    where type PathType Units JSONText = Path_Units JSONText
          toLens (Path_Units_View _) = viewLens :: Lens' Units JSONText
          toLens u = error $ ("Unexpected goal JSONText for Units: " ++ show u)
instance Path Units Units
    where type PathType Units Units = Path_Units Units
          toLens _ = iso id id
instance Path UserId UserId
    where type PathType UserId UserId = Path_UserId UserId
          toLens _ = iso id id
instance Path UserIds JSONText
    where type PathType UserIds JSONText = Path_UserIds JSONText
          toLens (Path_UserIds_View v) = (viewLens :: Lens' ([UserId])
                                                            Text) . toLens v
          toLens u = error $ ("Unexpected goal JSONText for [UserId] (aka UserIds): " ++ show u)
instance Path UserIds Text
    where type PathType UserIds Text = Path_UserIds Text
          toLens (Path_UserIds_View _) = viewLens :: Lens' ([UserId]) Text
          toLens u = error $ ("Unexpected goal Text for [UserId] (aka UserIds): " ++ show u)
instance Path UserIds UserIds
    where type PathType UserIds UserIds = Path_UserIds UserIds
          toLens _ = iso id id
