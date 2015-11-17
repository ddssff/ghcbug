{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances, TemplateHaskell, TypeSynonymInstances #-}
module Appraisal.CIString
    ( CIString(unCIString, CIString)
    , lens_CIString_Text
    ) where

import Control.Lens (Lens', iso)
import Data.Char (toLower)
import Data.Function (on)
import Data.Generics (Data, Typeable)
import Data.Monoid ((<>))
--import Data.SafeCopy (base, deriveSafeCopy)
import Data.String (IsString(fromString))
import qualified Data.Text as T

newtype CIString = CIString {unCIString :: String} deriving (Data, Typeable, Read, Show)

instance Ord CIString where
    compare = compare `on` (map toLower . unCIString)

instance Eq CIString where
    a == b = compare a b == EQ

instance Monoid CIString where
    mempty = CIString ""
    mappend (CIString a) (CIString b) = CIString (a <> b)

instance IsString CIString where
    fromString = CIString . T.unpack . T.strip . T.pack

-- | Like Data.String.IsString
class IsText a where
    fromText :: T.Text -> a

instance IsText T.Text where
    fromText = id

instance IsText CIString where
    fromText = fromString . T.unpack . T.strip

lens_CIString_Text :: Lens' CIString T.Text
lens_CIString_Text = iso (T.pack . unCIString) fromText
{-
$(deriveSafeCopy 1 'base ''CIString)
-}
