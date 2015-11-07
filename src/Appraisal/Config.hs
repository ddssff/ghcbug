{-# LANGUAGE CPP, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Appraisal.Config
    ( -- WebConf(..)
      AVRVersion(..)
    , title
    , screenDPI
    , printerDPI
    , Paths(ver, top, state, images, reports)
    , Top(Top)
    , PathsMonad(ver', top', state', images', reports')
    , imagesURIPathComponent
    , reportsURIPathComponent
    , topURIPath
    , topURI
    , reportsURIPath
    , reportsURI
    , imagesURIPath
    , imagesURI
    , imagesDir
    ) where

import Appraisal.File (FileCacheTop(FileCacheTop))
import Control.Monad.Reader (MonadReader, ask)
import System.FilePath ((</>))
import Network.URI (nullURI, URI(..))

data AVRVersion = AVR1 | AVR2 | AVR3

-- |These values are used to choose the best scale factor to use on
-- the images.
screenDPI = 100.0 :: Double
printerDPI = 600.0 :: Double

topURIPath = "/"
topURI = nullURI {uriPath = topURIPath}

class Paths a where
    ver :: a -> AVRVersion
    top :: a -> FilePath
    state :: a -> FilePath
    images :: a -> FileCacheTop
    reports :: a -> FilePath


newtype Top = Top FilePath

instance Paths Top where
    ver _ = AVR2
    top (Top x) = x
    state x = top x </> "_state"
    images x = FileCacheTop $ top x </> "images"
    reports x = top x </> "reports"

-- | Create a monad @PathsMonad@ from a reader monad for an instance of @Paths@.
class (Monad m, Applicative m, Paths p, MonadReader p m) => PathsMonad p m | m -> p where
    ver' :: m AVRVersion
    ver' = ver <$> ask
    top' :: m FilePath
    top' = top <$> ask
    state' :: m FilePath
    state' = state <$> ask
    images' :: m FileCacheTop
    images' = images <$> ask
    reports' :: m FilePath
    reports' = reports <$> ask

title :: Paths a => a -> String
title paths =
    case ver paths of
      AVR1 -> "Art Value Report Generator"
      AVR2 -> "Appraisal Report Online"
      AVR3 -> "AppraisalScribe"

-- The subdirectory of home containing the cached images
imagesURIPathComponent paths =
    case ver paths of
      AVR1 -> "images"
      AVR2 -> "images"
      AVR3 -> "images"

-- The subdirectory of home containing the cached images
reportsURIPathComponent paths =
    case ver paths of
      AVR1 -> "reports"
      AVR2 -> "reports"
      AVR3 -> "reports"

reportsURIPath v = topURIPath </> reportsURIPathComponent v
reportsURI v = nullURI {uriPath = reportsURIPath v}

-- The images are kept in a subdirectory
imagesURIPath v = topURIPath </> imagesURIPathComponent v
imagesURI v = nullURI {uriPath = imagesURIPath v}

imagesDir v = top v </> imagesURIPathComponent v

-- The local pathname of the directory containing the cached data files.
--reportDataPath top = top
{-
color1 = "#d4d4d0"
color2 = "#FCFCFF"
color3 = "#8080d0"
color4 = "#eeeecc"
-}

--breakAttrs = [intAttr "colspan" 3, strAttr "bgcolor" color1]
