{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             ScopedTypeVariables, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
-- |A data structure representing a local cache of a data file.  The
-- cached file persists across runs of our application, and can be
-- accessed by name and passed to software which requires a file, for
-- example a document formatter such as LaTeX.  The original data can
-- be supplied as either a URI, a local file path, or as a ByteString.
-- The file is then downloaded and stored on the local machine at a
-- location based on the file's checksum.
module Appraisal.File
    ( module Network.URI
    , MonadFileCacheTop(fileCacheTop)
    , FileCacheTop(..)
    , Checksum
    , File(..)
    , FileSource(..)
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Reader (ReaderT, ask)
import Data.Generics (Data(..), Typeable)
import Data.Monoid ((<>))
import Network.URI (URI(..), URIAuth(..), parseRelativeReference, parseURI)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

newtype FileCacheTop = FileCacheTop {unFileCacheTop :: FilePath} deriving Show

class Monad m => MonadFileCacheTop m where
    fileCacheTop :: m FilePath

instance Monad m => MonadFileCacheTop (ReaderT FileCacheTop m) where
    fileCacheTop  = ask >>= \ (FileCacheTop x) -> return x

-- |The original source if the file is saved, in case
-- the cache needs to be reconstructed.  However, we don't
-- store the original ByteString if that is all we began
-- with, that would be redundant and wasteful.
data FileSource
    = TheURI String
    | ThePath FilePath
    deriving (Show, Read, Eq, Ord, Data, Typeable)

-- | A type to represent a checksum which (unlike MD5Digest) is an instance of Data.
type Checksum = String

-- |A local cache of a file obtained from a 'FileSource'.
data File
    = File { fileSource :: Maybe FileSource     -- ^ Where the file's contents came from
           , fileChksum :: Checksum             -- ^ The checksum of the file's contents
           , fileMessages :: [String]           -- ^ Messages received while manipulating the file
           } deriving (Show, Read, Eq, Ord, Data, Typeable)

instance Pretty File where
    pPrint (File _ cksum _) = text ("File(" <> show cksum <> ")")

{-
$(deriveSafeCopy 1 'base ''File)
$(deriveSafeCopy 1 'base ''FileSource)
$(deriveSafeCopy 0 'base ''URI)
$(deriveSafeCopy 0 'base ''URIAuth)
-}
