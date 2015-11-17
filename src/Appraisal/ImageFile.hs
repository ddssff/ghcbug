{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             OverloadedStrings, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
-- |Support for a 'File' that contains an image.
module Appraisal.ImageFile
    ( ImageFile(..)
    , ImageType(..)
    , extension
    ) where

--import Appraisal.Exif (normalizeOrientationCode)
import Appraisal.File (File(..))
import Appraisal.Image (PixmapShape(..))
import Data.Generics (Data(..), Typeable)
import Data.Monoid ((<>))
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

data ImageFile
    = ImageFile
      { imageFile :: File
      , imageFileType :: ImageType
      , imageFileWidth :: Int
      , imageFileHeight :: Int
      , imageFileMaxVal :: Int
      } deriving (Show, Read, Eq, Ord, Data, Typeable)

data ImageType = PPM | JPEG | GIF | PNG deriving (Show, Read, Eq, Ord, Typeable, Data)

instance PixmapShape ImageFile where
    pixmapHeight = imageFileHeight
    pixmapWidth = imageFileWidth
    pixmapMaxVal = imageFileMaxVal

instance Pretty ImageFile where
    pPrint (ImageFile f typ w h _mx) = text "ImageFile(" <> pPrint f <> text (" " <> show w <> "x" <> show h <> " " <> show typ <> ")")

extension :: ImageType -> String
extension JPEG = ".jpg"
extension PPM = ".ppm"
extension GIF = ".gif"
extension PNG = ".png"

{-
$(deriveSafeCopy 0 'base ''ImageType)
$(deriveSafeCopy 0 'base ''ImageFile)
-}
