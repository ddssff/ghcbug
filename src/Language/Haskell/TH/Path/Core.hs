{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
module Language.Haskell.TH.Path.Core
    ( -- * Type classes and associated types
         Path(PathType, toLens)
    -- , PathType
    , IdPath(idPath)

    -- * Basic Path Types
    , Path_Pair(Path_First, Path_Second)
    -- , lens_mapPair
    -- , lens_mapFirst
    -- , lens_mapSecond
    , Path_Maybe(..)
    -- , lens_Maybe_Monoid
    -- , lens_Monoid_Maybe
    , Path_Map(..)
    , mat
    , Path_OMap(..)
    -- , Path_List(..)
    -- , at
    -- , lens_list
    , Path_Either(..)
    -- , eitherIso

    -- * Basic lenses
    , readOnlyLens
    , readShowLens
    -- , lens_trace
    -- , listLookupLens
    , lens_mrs
    -- , idLens
    -- , dummyLens
    -- , textLens
    -- , IsText(textLens')
    -- , stringLens
    , lens_UserIds_Text

    -- , pathTypeNames
    ) where

import Control.Applicative.Error (maybeRead)
import Control.Lens (Traversal', Lens', _Just, iso, lens)
import Data.Generics (Data, Typeable)
import Data.List as List (map)
import qualified Data.Map as M (Map, insert, lookup)
import Data.Maybe (catMaybes)
import Data.Text as Text (Text, pack, unpack, unwords, words)
import Data.UserId (UserId(..))
import Language.Haskell.TH.Instances ()
import Prelude hiding (exp)
import Safe (readMay)

-- | If there is an instance of 'Path' for a pair of types @s@ and
-- @a@, that means there is at least one way to obtain an @a@ from an
-- @s@.
class Path s a where
    type PathType s a
    -- ^ Each instance defines this type function which returns the
    -- path type.  Each value of this type represents a different way
    -- of obtaining the @a@ from the @s@.  For example, if @s@ is a
    -- record with two fields of type 'Int', the type @PathType s Int@
    -- would have distinct values for those two fields, and the lenses
    -- returned by 'toLens' would access those two fields.
    toLens :: PathType s a -> Traversal' s a
    -- ^ Function to turn a PathType into a lens to access (one of)
    -- the @a@ values.

class IdPath s where
    idPath :: s -- ^ The identity path for type s.  @toLens idPath@
                -- returns @iso id id@.

-- instance OrderKey k => Path (Order k a) a where
--     type PathType (Order k a) a = (Path_OMap k a)
--     toLens (Path_At k a) = lens_omat k . toLens a

-- Primitive path types

-- | A path type with constructors to extract either @fst@, @snd@, or
-- the pair itself.
data Path_Pair a b = Path_First a | Path_Second b | Path_Pair deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Either a b = Path_Left a | Path_Right b | Path_Either deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Invalid = Path_Invalid deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Maybe a = Path_Just a | Path_Maybe deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Map k v = Path_Look k v | Path_Map  deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_List a = Path_List deriving (Eq, Ord, Read, Show, Typeable, Data) -- No element lookup path - too dangerous, use OMap
data Path_OMap k a = Path_OMap | Path_At k a deriving (Eq, Ord, Read, Show, Typeable, Data)

instance IdPath (Path_Pair a b) where idPath = Path_Pair
instance IdPath (Path_Maybe a) where idPath = Path_Maybe
instance IdPath (Path_Either a b) where idPath = Path_Either
instance IdPath (Path_Map k v) where idPath = Path_Map
instance IdPath (Path_List a) where idPath = Path_List
instance IdPath (Path_OMap k a) where idPath = Path_OMap

-- | A lens that assumes usable round trip Read/Show instances for
-- a.  Similar to an isomorphism, but fails silently.
readShowLens :: (Show a, Read a) => Lens' a String
readShowLens = lens show (\r v ->
                           case maybeRead v of
                             Nothing -> r
                             Just r' -> r')

-- | A lens for 'Maybe' values whose getter turns Nothing into the
-- empty string and whose setter returns Nothing whenever read fails.
lens_mrs :: (Show a, Read a) => Lens' (Maybe a) String
lens_mrs = lens getter setter
  where getter Nothing = ""
        getter (Just x) = show x
        setter _ x = maybeRead x

readOnlyLens :: Lens' a a
readOnlyLens = iso id (error "Lens.readOnlyLens: TROUBLE ignoring write to readOnlyLens")

mat :: forall k a. (Show k, Ord k) => k -> Traversal' (M.Map k a) a
mat k = lens (M.lookup k) (\ mp ma -> maybe mp (\ a -> M.insert k a mp) ma) . _Just

class IsText a where
    textLens' :: Lens' a Text
    stringLens :: IsText a => Lens' a String
    stringLens = textLens' . iso unpack pack

lens_UserIds_Text :: Lens' [UserId] Text
lens_UserIds_Text = iso (encode') (decode')
    where
      decode' :: Text -> [UserId]
      decode' t =
          catMaybes . List.map readId . Text.words $ t
          where readId :: Text -> Maybe UserId
                readId = fmap UserId . readMay . unpack

      encode' :: [UserId] -> Text
      encode' uids =
          Text.unwords . List.map showId $ uids
          where showId = Text.pack . show . _unUserId
