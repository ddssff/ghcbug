{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             OverloadedStrings, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
-- |Support for a 'File' that contains an image.
module Appraisal.ImageFile
    ( ImageFile(..)
    , ImageType(..)
    , extension
    ) where

import Appraisal.Exif (normalizeOrientationCode)
import Appraisal.File (MonadFileCacheTop, File(..){-, fileCachePath, loadBytes, fileFromBytes, fileFromPath, fileFromURI, fileFromFile, fileFromCmd, fileFromCmdViaTemp-})
import Appraisal.Image (PixmapShape(..), ImageCrop(..))
import Appraisal.Utils.ErrorWithIO (logException, ensureLink)
import Control.Exception (catch, IOException, SomeException, throw)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Except (MonadError, catchError)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.Generics (Data(..), Typeable)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.SafeCopy (deriveSafeCopy, base)
import qualified Data.ByteString.Lazy as P (fromStrict, toStrict)
#ifdef LAZYIMAGES
import qualified Data.ByteString.Lazy as P
#else
import qualified Data.ByteString.UTF8 as P
import qualified Data.ByteString as P
#endif
import Network.URI (URI, uriToString)
import Numeric (showFFloat)
import System.Exit (ExitCode(..))
import System.Log.Logger (logM, Priority(ERROR))
import System.Process (CreateProcess(..), CmdSpec(..), proc, showCommandForUser, StdStream)
import System.Process.ListLike (readCreateProcessWithExitCode, readProcessWithExitCode, showCreateProcessForUser)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)
import Text.Regex (mkRegex, matchRegex)

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

$(deriveSafeCopy 0 'base ''ImageType)
$(deriveSafeCopy 0 'base ''ImageFile)
